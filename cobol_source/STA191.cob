       IDENTIFICATION DIVISION.
       PROGRAM-ID. STA191R.
      **********************************************  Z-WIN-RPG2   ****
      **OBS ved endring excel på Report Web *****************
      * * UPSI 0,1,2 BRUKES TIL RBS ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: STA191.rpg
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
           SELECT PRTFILE
               ASSIGN TO UT-S-PRTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRTFILE-STATUS.
           SELECT STATTAB
               ASSIGN TO STATTAB
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS STATTAB-STATUS
               RECORD KEY IS STATTAB-KEY1.
           SELECT PRTHND
               ASSIGN TO UT-S-PRTHND
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRTHND-STATUS.
           SELECT PRTKAT
               ASSIGN TO UT-S-PRTKAT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRTKAT-STATUS.
           SELECT PRTFIR
               ASSIGN TO UT-S-PRTFIR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRTFIR-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PARFILE
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARFILE-IO-AREA.
           05  PARFILE-IO-AREA-X           PICTURE X(80).
       FD PRTFILE
               BLOCK CONTAINS 4200
               RECORD CONTAINS 210.
       01  PRTFILE-IO-AREA.
           05  PRTFILE-IO-AREA-X           PICTURE X(210).
       FD STATTAB
               RECORD CONTAINS 40.
       01  STATTAB-IO-AREA.
           05  STATTAB-IO-AREA-X.
               10  STATTAB-KEY1            PICTURE X(8).
               10  FILLER                  PICTURE X(32).
       FD PRTHND
               BLOCK CONTAINS 4200
               RECORD CONTAINS 210.
       01  PRTHND-IO-AREA.
           05  PRTHND-IO-AREA-X            PICTURE X(210).
       FD PRTKAT
               BLOCK CONTAINS 4200
               RECORD CONTAINS 210.
       01  PRTKAT-IO-AREA.
           05  PRTKAT-IO-AREA-X            PICTURE X(210).
       FD PRTFIR
               BLOCK CONTAINS 4200
               RECORD CONTAINS 210.
       01  PRTFIR-IO-AREA.
           05  PRTFIR-IO-AREA-X            PICTURE X(210).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
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
           10  PARFILE-STATUS              PICTURE 99 VALUE 0.
           10  PRTFILE-STATUS              PICTURE 99 VALUE 0.
           10  STATTAB-STATUS              PICTURE 99 VALUE 0.
           10  PRTHND-STATUS               PICTURE 99 VALUE 0.
           10  PRTKAT-STATUS               PICTURE 99 VALUE 0.
           10  PRTFIR-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
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
           05  STATTAB-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
               10  MNDNA                   PICTURE X(9).
               10  AAR                     PICTURE X(4).
           05  PRTFILE-LEVEL-02.
               10  PRTFILE-02-L4.
                   15  PRTFILE-02-L4-FIRMA PICTURE X(3).
               10  PRTFILE-02-L3.
                   15  PRTFILE-02-L3-KAT   PICTURE X(3).
               10  PRTFILE-02-L2.
                   15  PRTFILE-02-L2-HDIST PICTURE X(3).
               10  PRTFILE-02-L1.
                   15  PRTFILE-02-L1-KUNDE PICTURE X(6).
           05  PRTFILE-DATA-FIELDS.
               10  KATHA                   PICTURE X(9).
               10  FAKAT                   PICTURE X(6).
               10  FIRMA                   PICTURE X(3).
               10  KAT                     PICTURE X(3).
               10  HDIST                   PICTURE X(3).
               10  ALFA                    PICTURE X(4).
               10  KUNDE                   PICTURE X(6).
               10  VGR                     PICTURE X(5).
               10  VGR1F                   PICTURE X(1).
               10  NAVN1                   PICTURE X(30).
               10  ADR                     PICTURE X(30).
               10  VGRNA                   PICTURE X(32).
               10  ANTDF-IO.
                   15  ANTDF               PICTURE S9(7).
               10  ANTDM-IO.
                   15  ANTDM               PICTURE S9(7).
               10  ANTAK-IO.
                   15  ANTAK               PICTURE S9(7).
               10  ANTAKF-IO.
                   15  ANTAKF              PICTURE S9(7).
               10  BELDM-IO.
                   15  BELDM               PICTURE S9(8).
               10  BELAK-IO.
                   15  BELAK               PICTURE S9(8).
               10  BELAKF-IO.
                   15  BELAKF              PICTURE S9(8).
               10  BELAV-IO.
                   15  BELAV               PICTURE S9(8).
               10  POSTNR                  PICTURE X(4).
               10  POSTST                  PICTURE X(15).
               10  BELDF-IO.
                   15  BELDF               PICTURE S9(8).
           05  STATTAB-DATA-FIELDS.
               10  SELGER                  PICTURE X(30).
           05  PRTHND-DATA-FIELDS.
               10  HKATHD                  PICTURE X(9).
               10  HFIRM                   PICTURE X(3).
               10  HALFA                   PICTURE X(4).
               10  HKUNDE                  PICTURE X(6).
               10  HVGR                    PICTURE X(5).
               10  HVG1F                   PICTURE X(1).
               10  HNAVN1                  PICTURE X(30).
               10  HADR                    PICTURE X(30).
               10  HVGRNA                  PICTURE X(32).
               10  HANTDF-IO.
                   15  HANTDF              PICTURE S9(7).
               10  HANTDM-IO.
                   15  HANTDM              PICTURE S9(7).
               10  HANTAK-IO.
                   15  HANTAK              PICTURE S9(7).
               10  HAAKF-IO.
                   15  HAAKF               PICTURE S9(7).
               10  HBELDM-IO.
                   15  HBELDM              PICTURE S9(8).
               10  HBELAK-IO.
                   15  HBELAK              PICTURE S9(8).
               10  HBAKF-IO.
                   15  HBAKF               PICTURE S9(8).
               10  HBELAV-IO.
                   15  HBELAV              PICTURE S9(8).
               10  HBELDF-IO.
                   15  HBELDF              PICTURE S9(8).
           05  PRTKAT-DATA-FIELDS.
               10  KFIKAT                  PICTURE X(6).
               10  KFIRM                   PICTURE X(3).
               10  KKAT                    PICTURE X(3).
               10  KHDIST                  PICTURE X(3).
               10  KALFA                   PICTURE X(4).
               10  KKUNDE                  PICTURE X(6).
               10  KVGR                    PICTURE X(5).
               10  KVG1F                   PICTURE X(1).
               10  KNAVN1                  PICTURE X(30).
               10  KADR                    PICTURE X(30).
               10  KVGRNA                  PICTURE X(32).
               10  KANTDF-IO.
                   15  KANTDF              PICTURE S9(7).
               10  KANTDM-IO.
                   15  KANTDM              PICTURE S9(7).
               10  KANTAK-IO.
                   15  KANTAK              PICTURE S9(7).
               10  KAAKF-IO.
                   15  KAAKF               PICTURE S9(7).
               10  KBELDM-IO.
                   15  KBELDM              PICTURE S9(8).
               10  KBELAK-IO.
                   15  KBELAK              PICTURE S9(8).
               10  KBAKF-IO.
                   15  KBAKF               PICTURE S9(8).
               10  KBELAV-IO.
                   15  KBELAV              PICTURE S9(8).
               10  KBELDF-IO.
                   15  KBELDF              PICTURE S9(8).
           05  PRTFIR-DATA-FIELDS.
               10  FFIKAT                  PICTURE X(6).
               10  FFIRM                   PICTURE X(3).
               10  FKAT                    PICTURE X(3).
               10  FHDIST                  PICTURE X(3).
               10  FALFA                   PICTURE X(4).
               10  FKUNDE                  PICTURE X(6).
               10  FVGR                    PICTURE X(5).
               10  FVG1F                   PICTURE X(1).
               10  FNAVN1                  PICTURE X(30).
               10  FADR                    PICTURE X(30).
               10  FVGRNA                  PICTURE X(32).
               10  FANTDF-IO.
                   15  FANTDF              PICTURE S9(7).
               10  FANTDM-IO.
                   15  FANTDM              PICTURE S9(7).
               10  FANTAK-IO.
                   15  FANTAK              PICTURE S9(7).
               10  FAAKF-IO.
                   15  FAAKF               PICTURE S9(7).
               10  FBELDM-IO.
                   15  FBELDM              PICTURE S9(8).
               10  FBELAK-IO.
                   15  FBELAK              PICTURE S9(8).
               10  FBAKF-IO.
                   15  FBAKF               PICTURE S9(8).
               10  FBELAV-IO.
                   15  FBELAV              PICTURE S9(8).
               10  FBELDF-IO.
                   15  FBELDF              PICTURE S9(8).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  F5                      PICTURE X(5).
               10  SKEY                    PICTURE X(8).
               10  L1ADM-IO.
                   15  L1ADM               PICTURE S9(8).
               10  L2ADM-IO.
                   15  L2ADM               PICTURE S9(8).
               10  L3ADM-IO.
                   15  L3ADM               PICTURE S9(8).
               10  L4ADM-IO.
                   15  L4ADM               PICTURE S9(8).
               10  L1AAK-IO.
                   15  L1AAK               PICTURE S9(8).
               10  L2AAK-IO.
                   15  L2AAK               PICTURE S9(8).
               10  L3AAK-IO.
                   15  L3AAK               PICTURE S9(8).
               10  L4AAK-IO.
                   15  L4AAK               PICTURE S9(8).
               10  L1AAKF-IO.
                   15  L1AAKF              PICTURE S9(8).
               10  L2AAKF-IO.
                   15  L2AAKF              PICTURE S9(8).
               10  L3AAKF-IO.
                   15  L3AAKF              PICTURE S9(8).
               10  L4AAKF-IO.
                   15  L4AAKF              PICTURE S9(8).
               10  L1BDM-IO.
                   15  L1BDM               PICTURE S9(9).
               10  L2BDM-IO.
                   15  L2BDM               PICTURE S9(9).
               10  L3BDM-IO.
                   15  L3BDM               PICTURE S9(9).
               10  L4BDM-IO.
                   15  L4BDM               PICTURE S9(9).
               10  L1BDMF-IO.
                   15  L1BDMF              PICTURE S9(9).
               10  L2BDMF-IO.
                   15  L2BDMF              PICTURE S9(9).
               10  L3BDMF-IO.
                   15  L3BDMF              PICTURE S9(9).
               10  L4BDMF-IO.
                   15  L4BDMF              PICTURE S9(9).
               10  L1BAK-IO.
                   15  L1BAK               PICTURE S9(9).
               10  L2BAK-IO.
                   15  L2BAK               PICTURE S9(9).
               10  L3BAK-IO.
                   15  L3BAK               PICTURE S9(9).
               10  L4BAK-IO.
                   15  L4BAK               PICTURE S9(9).
               10  L1BAKF-IO.
                   15  L1BAKF              PICTURE S9(9).
               10  L2BAKF-IO.
                   15  L2BAKF              PICTURE S9(9).
               10  L3BAKF-IO.
                   15  L3BAKF              PICTURE S9(9).
               10  L4BAKF-IO.
                   15  L4BAKF              PICTURE S9(9).
               10  L1BAV-IO.
                   15  L1BAV               PICTURE S9(9).
               10  L2BAV-IO.
                   15  L2BAV               PICTURE S9(9).
               10  L3BAV-IO.
                   15  L3BAV               PICTURE S9(9).
               10  L4BAV-IO.
                   15  L4BAV               PICTURE S9(9).
               10  KUNVG1                  PICTURE X(1).
               10  SKADM-IO.
                   15  SKADM               PICTURE S9(8).
               10  SKAAK-IO.
                   15  SKAAK               PICTURE S9(8).
               10  SKAAKF-IO.
                   15  SKAAKF              PICTURE S9(8).
               10  SKBDM-IO.
                   15  SKBDM               PICTURE S9(9).
               10  SKBDMF-IO.
                   15  SKBDMF              PICTURE S9(9).
               10  SKBAK-IO.
                   15  SKBAK               PICTURE S9(9).
               10  SKBAKF-IO.
                   15  SKBAKF              PICTURE S9(9).
               10  SKBAV-IO.
                   15  SKBAV               PICTURE S9(9).
               10  HNDVG1                  PICTURE X(1).
               10  SHADM-IO.
                   15  SHADM               PICTURE S9(8).
               10  SHAAK-IO.
                   15  SHAAK               PICTURE S9(8).
               10  SHAAKF-IO.
                   15  SHAAKF              PICTURE S9(8).
               10  SHBDM-IO.
                   15  SHBDM               PICTURE S9(9).
               10  SHBDMF-IO.
                   15  SHBDMF              PICTURE S9(9).
               10  SHBAK-IO.
                   15  SHBAK               PICTURE S9(9).
               10  SHBAKF-IO.
                   15  SHBAKF              PICTURE S9(9).
               10  SHBAV-IO.
                   15  SHBAV               PICTURE S9(9).
               10  SELVG1                  PICTURE X(1).
               10  L5ADM-IO.
                   15  L5ADM               PICTURE S9(8).
               10  L5AAK-IO.
                   15  L5AAK               PICTURE S9(8).
               10  L5AAKF-IO.
                   15  L5AAKF              PICTURE S9(8).
               10  L5BDM-IO.
                   15  L5BDM               PICTURE S9(9).
               10  L5BDMF-IO.
                   15  L5BDMF              PICTURE S9(9).
               10  L5BAK-IO.
                   15  L5BAK               PICTURE S9(9).
               10  L5BAKF-IO.
                   15  L5BAKF              PICTURE S9(9).
               10  L5BAV-IO.
                   15  L5BAV               PICTURE S9(9).
               10  FIRVG1                  PICTURE X(1).
               10  L6ADM-IO.
                   15  L6ADM               PICTURE S9(8).
               10  L6AAK-IO.
                   15  L6AAK               PICTURE S9(8).
               10  L6AAKF-IO.
                   15  L6AAKF              PICTURE S9(8).
               10  L6BDM-IO.
                   15  L6BDM               PICTURE S9(9).
               10  L6BDMF-IO.
                   15  L6BDMF              PICTURE S9(9).
               10  L6BAK-IO.
                   15  L6BAK               PICTURE S9(9).
               10  L6BAKF-IO.
                   15  L6BAKF              PICTURE S9(9).
               10  L6BAV-IO.
                   15  L6BAV               PICTURE S9(9).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-70YY9R               PICTURE Z.ZZZ.ZZ9-.
               10  XO-80YY9R               PICTURE ZZ.ZZZ.ZZ9-.
               10  EDIT-SKADM              PICTURE ZZZZZ.ZZ9-.
               10  EDIT-SKAAK              PICTURE ZZZZZ.ZZ9-.
               10  EDIT-SKAAKF             PICTURE ZZZZZ.ZZ9-.
               10  XO-90YY9R               PICTURE ZZZ.ZZZ.ZZ9-.
               10  EDIT-L1ADM              PICTURE ZZZZZ.ZZ9-.
               10  EDIT-L1AAK              PICTURE ZZZZZ.ZZ9-.
               10  EDIT-L1AAKF             PICTURE ZZZZZ.ZZ9-.
               10  EDIT-SHADM              PICTURE ZZZZZ.ZZ9-.
               10  EDIT-SHAAK              PICTURE ZZZZZ.ZZ9-.
               10  EDIT-SHAAKF             PICTURE ZZZZZ.ZZ9-.
               10  EDIT-L2ADM              PICTURE ZZZZZ.ZZ9-.
               10  EDIT-L2AAK              PICTURE ZZZZZ.ZZ9-.
               10  EDIT-L2AAKF             PICTURE ZZZZZ.ZZ9-.
               10  EDIT-L3ADM              PICTURE ZZZZZ.ZZ9-.
               10  EDIT-L3AAK              PICTURE ZZZZZ.ZZ9-.
               10  EDIT-L3AAKF             PICTURE ZZZZZ.ZZ9-.
               10  EDIT-L5ADM              PICTURE ZZZZZ.ZZ9-.
               10  EDIT-L5AAK              PICTURE ZZZZZ.ZZ9-.
               10  EDIT-L5AAKF             PICTURE ZZZZZ.ZZ9-.
               10  EDIT-L6ADM              PICTURE ZZZZZ.ZZ9-.
               10  EDIT-L6AAK              PICTURE ZZZZZ.ZZ9-.
               10  EDIT-L6AAKF             PICTURE ZZZZZ.ZZ9-.
               10  EDIT-L4ADM              PICTURE ZZZZZ.ZZ9-.
               10  EDIT-L4AAK              PICTURE ZZZZZ.ZZ9-.
               10  EDIT-L4AAKF             PICTURE ZZZZZ.ZZ9-.
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
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-07                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
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
 
           IF  PARFILE-PROCESS
               PERFORM PARFILE-IDSET
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
           PERFORM TOTAL-CALCS.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           PERFORM HEADING-OVERFLOW
 
           IF  PARFILE-PROCESS
               PERFORM PARFILE-FLDSET
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
           IF  (I-L4)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-86)
               GO TO SLUTT-T
           END-IF
           IF  (I-L4)
               SUBTRACT PAGE0              FROM PAGE0
      *******************************************************
      * RUTINE FOR HENTING AV SELGERNAVN.                   *
      *******************************************************
           END-IF
           IF  (I-L3)
               MOVE FIRMA                  TO F5 (1:3)
               MOVE '01'                   TO F5 (4:2)
               MOVE F5                     TO SKEY (1:5)
               MOVE KAT                    TO SKEY (6:3)
               MOVE SKEY                   TO STATTAB-KEY1
               READ STATTAB RECORD KEY IS STATTAB-KEY1
               INVALID KEY
                   SET I-10                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-10            TO TRUE
                   PERFORM STATTAB-FLDSET
                   PERFORM STATTAB-IDSET
               END-READ
      ********************************************************
           END-IF
           IF  (I-02)
               ADD ANTDM                   TO L1ADM
               ADD ANTDM                   TO L2ADM
               ADD ANTDM                   TO L3ADM
               ADD ANTDM                   TO L4ADM
               ADD ANTAK                   TO L1AAK
               ADD ANTAK                   TO L2AAK
               ADD ANTAK                   TO L3AAK
               ADD ANTAK                   TO L4AAK
               ADD ANTAKF                  TO L1AAKF
               ADD ANTAKF                  TO L2AAKF
               ADD ANTAKF                  TO L3AAKF
               ADD ANTAKF                  TO L4AAKF
               ADD BELDM                   TO L1BDM
               ADD BELDM                   TO L2BDM
               ADD BELDM                   TO L3BDM
               ADD BELDM                   TO L4BDM
               ADD BELDF                   TO L1BDMF
               ADD BELDF                   TO L2BDMF
               ADD BELDF                   TO L3BDMF
               ADD BELDF                   TO L4BDMF
               ADD BELAK                   TO L1BAK
               ADD BELAK                   TO L2BAK
               ADD BELAK                   TO L3BAK
               ADD BELAK                   TO L4BAK
               ADD BELAKF                  TO L1BAKF
               ADD BELAKF                  TO L2BAKF
               ADD BELAKF                  TO L3BAKF
               ADD BELAKF                  TO L4BAKF
               ADD BELAV                   TO L1BAV
               ADD BELAV                   TO L2BAV
               ADD BELAV                   TO L3BAV
               ADD BELAV                   TO L4BAV
      ***********************************************************
      * SUBTOTALER  PR.  KUNDE.                                 *
      ***********************************************************
           END-IF
           IF  (I-L1)
               MOVE VGR1F                  TO KUNVG1
           END-IF
           IF  (I-02)
               SET NOT-I-37                TO TRUE
               IF  VGR1F NOT = KUNVG1
                   SET I-37                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-37)
               PERFORM EXCEPTION-OUTPUT
               MOVE VGR1F                  TO KUNVG1
           END-IF
           IF  (I-02)
               SET NOT-I-37                TO TRUE
               ADD ANTDM                   TO SKADM
               ADD ANTAK                   TO SKAAK
               ADD ANTAKF                  TO SKAAKF
               ADD BELDM                   TO SKBDM
               ADD BELDF                   TO SKBDMF
               ADD BELAK                   TO SKBAK
               ADD BELAKF                  TO SKBAKF
               ADD BELAV                   TO SKBAV
      ***********************************************************
           END-IF
           .
 
       SLUTT-T.
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'STA33'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'STA191  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF
           IF  (I-U2)
               SET NOT-I-86                TO TRUE
               IF  LANTX < 2
                   SET I-86                TO TRUE
               END-IF
           END-IF.
      ******************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L0 AND I-86)
               GO TO L1END-T
      ***********************************************************
      * T O T A L E R   PR. KUNDE.                              *
      ***********************************************************
           END-IF
           IF  (I-L1)
               SET I-31                    TO TRUE
               SET I-37                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-31                TO TRUE
               SET NOT-I-37                TO TRUE
      ***********************************************************
      * T O T A L E R   PR. HND.DIST.                           *
      ***********************************************************
           END-IF
           IF  (I-L2)
               SET I-32                    TO TRUE
               SET I-70                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-32                TO TRUE
      *** LES AV DEMANDFILE PR HND.DIST   ***
           END-IF
           IF  (I-L2 AND NOT-I-44)
               READ PRTHND
               AT END
                   SET I-45                TO TRUE
               NOT AT END
                   SET NOT-I-45            TO TRUE
                   PERFORM PRTHND-FLDSET
                   PERFORM PRTHND-IDSET
               END-READ
               MOVE HVG1F                  TO HNDVG1
           END-IF
           IF  (I-L2)
               SET I-44                    TO TRUE
           END-IF.
 
       LOOP1-T.
           IF  (I-L2)
               SET NOT-I-47                TO TRUE
               IF  HVG1F NOT = HNDVG1
                   SET I-47                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-47)
               PERFORM EXCEPTION-OUTPUT
               MOVE HVG1F                  TO HNDVG1
           END-IF
           IF  (I-L2)
               SET NOT-I-47                TO TRUE
               SET NOT-I-46                TO TRUE
               IF  KATHA = HKATHD
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-46)
               GO TO SLUTL2-T
           END-IF
           IF  (I-L2)
               ADD HANTDM                  TO SHADM
               ADD HANTAK                  TO SHAAK
               ADD HAAKF                   TO SHAAKF
               ADD HBELDM                  TO SHBDM
               ADD HBELDF                  TO SHBDMF
               ADD HBELAK                  TO SHBAK
               ADD HBAKF                   TO SHBAKF
               ADD HBELAV                  TO SHBAV
           END-IF
           IF  (I-L2 AND I-46 AND I-OF)
               SET I-32                    TO TRUE
           END-IF
           IF  (I-L2 AND I-46)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L2)
               SET NOT-I-46                TO TRUE
               SET NOT-I-32                TO TRUE
               READ PRTHND
               AT END
                   SET I-45                TO TRUE
               NOT AT END
                   SET NOT-I-45            TO TRUE
                   PERFORM PRTHND-FLDSET
                   PERFORM PRTHND-IDSET
               END-READ
           END-IF
           IF  (I-L2 AND NOT-I-45)
               GO TO LOOP1-T
           END-IF
           IF  (I-L2 AND I-45)
               SET I-47                    TO TRUE
           END-IF
           IF  (I-L2 AND I-45 AND I-47)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L2)
               SET NOT-I-47                TO TRUE
           END-IF.
 
       SLUTL2-T.
           IF  (I-L2)
               SET I-22                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-22                TO TRUE
               SET NOT-I-70                TO TRUE
      ***********************************************************
      * T O T A L E R   PR. SELGER.                             *
      ***********************************************************
           END-IF
           IF  (I-L3)
               SET I-33                    TO TRUE
               SET I-70                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-33                TO TRUE
      *** LES AV DEMANDFILE PR KATEGORI   ***
           END-IF
           IF  (I-L3 AND NOT-I-54)
               READ PRTKAT
               AT END
                   SET I-55                TO TRUE
               NOT AT END
                   SET NOT-I-55            TO TRUE
                   PERFORM PRTKAT-FLDSET
                   PERFORM PRTKAT-IDSET
               END-READ
               MOVE KVG1F                  TO SELVG1
           END-IF
           IF  (I-L3)
               SET I-54                    TO TRUE
           END-IF.
 
       LOOP2-T.
           IF  (I-L3)
               SET NOT-I-57                TO TRUE
               IF  KVG1F NOT = SELVG1
                   SET I-57                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-57)
               PERFORM EXCEPTION-OUTPUT
               MOVE KVG1F                  TO SELVG1
           END-IF
           IF  (I-L3)
               SET NOT-I-57                TO TRUE
               SET NOT-I-56                TO TRUE
               IF  FAKAT = KFIKAT
                   SET I-56                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-56)
               GO TO SLUTL3-T
           END-IF
           IF  (I-L3)
               ADD KANTDM                  TO L5ADM
               ADD KANTAK                  TO L5AAK
               ADD KAAKF                   TO L5AAKF
               ADD KBELDM                  TO L5BDM
               ADD KBELDF                  TO L5BDMF
               ADD KBELAK                  TO L5BAK
               ADD KBAKF                   TO L5BAKF
               ADD KBELAV                  TO L5BAV
           END-IF
           IF  (I-L3 AND I-56 AND I-OF)
               SET I-33                    TO TRUE
           END-IF
           IF  (I-L3 AND I-56)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L3)
               SET NOT-I-56                TO TRUE
               SET NOT-I-33                TO TRUE
               READ PRTKAT
               AT END
                   SET I-55                TO TRUE
               NOT AT END
                   SET NOT-I-55            TO TRUE
                   PERFORM PRTKAT-FLDSET
                   PERFORM PRTKAT-IDSET
               END-READ
           END-IF
           IF  (I-L3 AND NOT-I-55)
               GO TO LOOP2-T
           END-IF
           IF  (I-L3 AND I-55)
               SET I-57                    TO TRUE
           END-IF
           IF  (I-L3 AND I-55 AND I-57)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L3)
               SET NOT-I-57                TO TRUE
           END-IF.
 
       SLUTL3-T.
           IF  (I-L3)
               SET I-23                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-23                TO TRUE
               SET NOT-I-70                TO TRUE
      ***********************************************************
      * T O T A L E R   PR. FIRMA.                              *
      ***********************************************************
           END-IF
           IF  (I-L4)
               SET I-34                    TO TRUE
               SET I-70                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-34                TO TRUE
      *** LES AV DEMANDFILE PR FIRMA ***
           END-IF
           IF  (I-L4 AND NOT-I-64)
               READ PRTFIR
               AT END
                   SET I-65                TO TRUE
               NOT AT END
                   SET NOT-I-65            TO TRUE
                   PERFORM PRTFIR-FLDSET
                   PERFORM PRTFIR-IDSET
               END-READ
               MOVE FVG1F                  TO FIRVG1
           END-IF
           IF  (I-L4)
               SET I-64                    TO TRUE
           END-IF.
 
       LOOPF-T.
           IF  (I-L4)
               SET NOT-I-67                TO TRUE
               IF  FVG1F NOT = FIRVG1
                   SET I-67                TO TRUE
               END-IF
           END-IF
           IF  (I-L4 AND I-67)
               PERFORM EXCEPTION-OUTPUT
               MOVE FVG1F                  TO FIRVG1
           END-IF
           IF  (I-L4)
               SET NOT-I-67                TO TRUE
               SET NOT-I-66                TO TRUE
               IF  FIRMA = FFIRM
                   SET I-66                TO TRUE
               END-IF
           END-IF
           IF  (I-L4 AND NOT-I-66)
               GO TO SLUTF4-T
           END-IF
           IF  (I-L4)
               ADD FANTDM                  TO L6ADM
               ADD FANTAK                  TO L6AAK
               ADD FAAKF                   TO L6AAKF
               ADD FBELDM                  TO L6BDM
               ADD FBELDF                  TO L6BDMF
               ADD FBELAK                  TO L6BAK
               ADD FBAKF                   TO L6BAKF
               ADD FBELAV                  TO L6BAV
           END-IF
           IF  (I-L4 AND I-66 AND I-OF)
               SET I-34                    TO TRUE
           END-IF
           IF  (I-L4 AND I-66)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L4)
               SET NOT-I-66                TO TRUE
               SET NOT-I-34                TO TRUE
               READ PRTFIR
               AT END
                   SET I-65                TO TRUE
               NOT AT END
                   SET NOT-I-65            TO TRUE
                   PERFORM PRTFIR-FLDSET
                   PERFORM PRTFIR-IDSET
               END-READ
           END-IF
           IF  (I-L4 AND NOT-I-65)
               GO TO LOOPF-T
           END-IF
           IF  (I-L4 AND I-65)
               SET I-67                    TO TRUE
           END-IF
           IF  (I-L4 AND I-65 AND I-67)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L4)
               SET NOT-I-67                TO TRUE
           END-IF.
 
       SLUTF4-T.
           IF  (I-L4)
               SET I-24                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-24                TO TRUE
               SET NOT-I-70                TO TRUE
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
               END-READ
           END-IF.
 
       PARFILE-FLDSET SECTION.
       PARFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PARFILE-IO-AREA (1:1) = 'P' )
               MOVE PARFILE-IO-AREA (2:9)  TO MNDNA (1:9)
               MOVE PARFILE-IO-AREA (11:4) TO AAR (1:4)
           END-EVALUATE.
 
       PARFILE-IDCHK SECTION.
       PARFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARFILE-IO-AREA (1:1) = 'P' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARFILE-IDSET SECTION.
       PARFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARFILE-IO-AREA (1:1) = 'P' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
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
               MOVE PRTFILE-IO-AREA (3:9)  TO KATHA (1:9)
               MOVE PRTFILE-IO-AREA (3:6)  TO FAKAT (1:6)
               MOVE PRTFILE-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE PRTFILE-IO-AREA (6:3)  TO KAT (1:3)
               MOVE PRTFILE-IO-AREA (9:3)  TO HDIST (1:3)
               MOVE PRTFILE-IO-AREA (12:4) TO ALFA (1:4)
               MOVE PRTFILE-IO-AREA (16:6) TO KUNDE (1:6)
               MOVE PRTFILE-IO-AREA (22:5) TO VGR (1:5)
               MOVE PRTFILE-IO-AREA (22:1) TO VGR1F (1:1)
               MOVE PRTFILE-IO-AREA (27:30) TO NAVN1 (1:30)
               MOVE PRTFILE-IO-AREA (57:30) TO ADR (1:30)
               MOVE PRTFILE-IO-AREA (87:32) TO VGRNA (1:32)
               MOVE PRTFILE-IO-AREA (121:7) TO ANTDF-IO
               INSPECT ANTDF-IO REPLACING ALL ' ' BY '0'
               MOVE PRTFILE-IO-AREA (128:7) TO ANTDM-IO
               INSPECT ANTDM-IO REPLACING ALL ' ' BY '0'
               MOVE PRTFILE-IO-AREA (135:7) TO ANTAK-IO
               INSPECT ANTAK-IO REPLACING ALL ' ' BY '0'
               MOVE PRTFILE-IO-AREA (142:7) TO ANTAKF-IO
               INSPECT ANTAKF-IO REPLACING ALL ' ' BY '0'
               MOVE PRTFILE-IO-AREA (149:8) TO BELDM-IO
               INSPECT BELDM-IO REPLACING ALL ' ' BY '0'
               MOVE PRTFILE-IO-AREA (157:8) TO BELAK-IO
               INSPECT BELAK-IO REPLACING ALL ' ' BY '0'
               MOVE PRTFILE-IO-AREA (165:8) TO BELAKF-IO
               INSPECT BELAKF-IO REPLACING ALL ' ' BY '0'
               MOVE PRTFILE-IO-AREA (173:8) TO BELAV-IO
               INSPECT BELAV-IO REPLACING ALL ' ' BY '0'
               MOVE PRTFILE-IO-AREA (181:4) TO POSTNR (1:4)
               MOVE PRTFILE-IO-AREA (185:15) TO POSTST (1:15)
               MOVE PRTFILE-IO-AREA (201:8) TO BELDF-IO
               INSPECT BELDF-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       PRTFILE-IDSET SECTION.
       PRTFILE-IDSET-P.
           SET I-02                        TO TRUE.
 
       PRTFILE-CHK-LEVEL SECTION.
       PRTFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO PRTFILE-LEVEL-02
               MOVE PRTFILE-IO-AREA (3:3)  TO PRTFILE-02-L4-FIRMA
               MOVE PRTFILE-IO-AREA (6:3)  TO PRTFILE-02-L3-KAT
               MOVE PRTFILE-IO-AREA (9:3)  TO PRTFILE-02-L2-HDIST
               MOVE PRTFILE-IO-AREA (16:6) TO PRTFILE-02-L1-KUNDE
               IF  PRTFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  PRTFILE-02-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  PRTFILE-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  PRTFILE-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  PRTFILE-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  PRTFILE-02-L4         TO THE-PRIOR-L4
               MOVE  PRTFILE-02-L3         TO THE-PRIOR-L3
               MOVE  PRTFILE-02-L2         TO THE-PRIOR-L2
               MOVE  PRTFILE-02-L1         TO THE-PRIOR-L1
               SET PRTFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       STATTAB-FLDSET SECTION.
       STATTAB-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE STATTAB-IO-AREA (11:30) TO SELGER (1:30)
           END-EVALUATE.
 
       STATTAB-IDSET SECTION.
       STATTAB-IDSET-P.
           SET I-07                        TO TRUE.
 
       PRTHND-FLDSET SECTION.
       PRTHND-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PRTHND-IO-AREA (3:9)   TO HKATHD (1:9)
               MOVE PRTHND-IO-AREA (3:3)   TO HFIRM (1:3)
               MOVE PRTHND-IO-AREA (12:4)  TO HALFA (1:4)
               MOVE PRTHND-IO-AREA (16:6)  TO HKUNDE (1:6)
               MOVE PRTHND-IO-AREA (22:5)  TO HVGR (1:5)
               MOVE PRTHND-IO-AREA (22:1)  TO HVG1F (1:1)
               MOVE PRTHND-IO-AREA (27:30) TO HNAVN1 (1:30)
               MOVE PRTHND-IO-AREA (57:30) TO HADR (1:30)
               MOVE PRTHND-IO-AREA (87:32) TO HVGRNA (1:32)
               MOVE PRTHND-IO-AREA (121:7) TO HANTDF-IO
               INSPECT HANTDF-IO REPLACING ALL ' ' BY '0'
               MOVE PRTHND-IO-AREA (128:7) TO HANTDM-IO
               INSPECT HANTDM-IO REPLACING ALL ' ' BY '0'
               MOVE PRTHND-IO-AREA (135:7) TO HANTAK-IO
               INSPECT HANTAK-IO REPLACING ALL ' ' BY '0'
               MOVE PRTHND-IO-AREA (142:7) TO HAAKF-IO
               INSPECT HAAKF-IO REPLACING ALL ' ' BY '0'
               MOVE PRTHND-IO-AREA (149:8) TO HBELDM-IO
               INSPECT HBELDM-IO REPLACING ALL ' ' BY '0'
               MOVE PRTHND-IO-AREA (157:8) TO HBELAK-IO
               INSPECT HBELAK-IO REPLACING ALL ' ' BY '0'
               MOVE PRTHND-IO-AREA (165:8) TO HBAKF-IO
               INSPECT HBAKF-IO REPLACING ALL ' ' BY '0'
               MOVE PRTHND-IO-AREA (173:8) TO HBELAV-IO
               INSPECT HBELAV-IO REPLACING ALL ' ' BY '0'
               MOVE PRTHND-IO-AREA (201:8) TO HBELDF-IO
               INSPECT HBELDF-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       PRTHND-IDSET SECTION.
       PRTHND-IDSET-P.
           SET I-03                        TO TRUE.
 
       PRTKAT-FLDSET SECTION.
       PRTKAT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PRTKAT-IO-AREA (3:6)   TO KFIKAT (1:6)
               MOVE PRTKAT-IO-AREA (3:3)   TO KFIRM (1:3)
               MOVE PRTKAT-IO-AREA (6:3)   TO KKAT (1:3)
               MOVE PRTKAT-IO-AREA (9:3)   TO KHDIST (1:3)
               MOVE PRTKAT-IO-AREA (12:4)  TO KALFA (1:4)
               MOVE PRTKAT-IO-AREA (16:6)  TO KKUNDE (1:6)
               MOVE PRTKAT-IO-AREA (22:5)  TO KVGR (1:5)
               MOVE PRTKAT-IO-AREA (22:1)  TO KVG1F (1:1)
               MOVE PRTKAT-IO-AREA (27:30) TO KNAVN1 (1:30)
               MOVE PRTKAT-IO-AREA (57:30) TO KADR (1:30)
               MOVE PRTKAT-IO-AREA (87:32) TO KVGRNA (1:32)
               MOVE PRTKAT-IO-AREA (121:7) TO KANTDF-IO
               INSPECT KANTDF-IO REPLACING ALL ' ' BY '0'
               MOVE PRTKAT-IO-AREA (128:7) TO KANTDM-IO
               INSPECT KANTDM-IO REPLACING ALL ' ' BY '0'
               MOVE PRTKAT-IO-AREA (135:7) TO KANTAK-IO
               INSPECT KANTAK-IO REPLACING ALL ' ' BY '0'
               MOVE PRTKAT-IO-AREA (142:7) TO KAAKF-IO
               INSPECT KAAKF-IO REPLACING ALL ' ' BY '0'
               MOVE PRTKAT-IO-AREA (149:8) TO KBELDM-IO
               INSPECT KBELDM-IO REPLACING ALL ' ' BY '0'
               MOVE PRTKAT-IO-AREA (157:8) TO KBELAK-IO
               INSPECT KBELAK-IO REPLACING ALL ' ' BY '0'
               MOVE PRTKAT-IO-AREA (165:8) TO KBAKF-IO
               INSPECT KBAKF-IO REPLACING ALL ' ' BY '0'
               MOVE PRTKAT-IO-AREA (173:8) TO KBELAV-IO
               INSPECT KBELAV-IO REPLACING ALL ' ' BY '0'
               MOVE PRTKAT-IO-AREA (201:8) TO KBELDF-IO
               INSPECT KBELDF-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       PRTKAT-IDSET SECTION.
       PRTKAT-IDSET-P.
           SET I-04                        TO TRUE.
 
       PRTFIR-FLDSET SECTION.
       PRTFIR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PRTFIR-IO-AREA (3:6)   TO FFIKAT (1:6)
               MOVE PRTFIR-IO-AREA (3:3)   TO FFIRM (1:3)
               MOVE PRTFIR-IO-AREA (6:3)   TO FKAT (1:3)
               MOVE PRTFIR-IO-AREA (9:3)   TO FHDIST (1:3)
               MOVE PRTFIR-IO-AREA (12:4)  TO FALFA (1:4)
               MOVE PRTFIR-IO-AREA (16:6)  TO FKUNDE (1:6)
               MOVE PRTFIR-IO-AREA (22:5)  TO FVGR (1:5)
               MOVE PRTFIR-IO-AREA (22:1)  TO FVG1F (1:1)
               MOVE PRTFIR-IO-AREA (27:30) TO FNAVN1 (1:30)
               MOVE PRTFIR-IO-AREA (57:30) TO FADR (1:30)
               MOVE PRTFIR-IO-AREA (87:32) TO FVGRNA (1:32)
               MOVE PRTFIR-IO-AREA (121:7) TO FANTDF-IO
               INSPECT FANTDF-IO REPLACING ALL ' ' BY '0'
               MOVE PRTFIR-IO-AREA (128:7) TO FANTDM-IO
               INSPECT FANTDM-IO REPLACING ALL ' ' BY '0'
               MOVE PRTFIR-IO-AREA (135:7) TO FANTAK-IO
               INSPECT FANTAK-IO REPLACING ALL ' ' BY '0'
               MOVE PRTFIR-IO-AREA (142:7) TO FAAKF-IO
               INSPECT FAAKF-IO REPLACING ALL ' ' BY '0'
               MOVE PRTFIR-IO-AREA (149:8) TO FBELDM-IO
               INSPECT FBELDM-IO REPLACING ALL ' ' BY '0'
               MOVE PRTFIR-IO-AREA (157:8) TO FBELAK-IO
               INSPECT FBELAK-IO REPLACING ALL ' ' BY '0'
               MOVE PRTFIR-IO-AREA (165:8) TO FBAKF-IO
               INSPECT FBAKF-IO REPLACING ALL ' ' BY '0'
               MOVE PRTFIR-IO-AREA (173:8) TO FBELAV-IO
               INSPECT FBELAV-IO REPLACING ALL ' ' BY '0'
               MOVE PRTFIR-IO-AREA (201:8) TO FBELDF-IO
               INSPECT FBELDF-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       PRTFIR-IDSET SECTION.
       PRTFIR-IDSET-P.
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
           IF  (I-02 AND I-L1 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* * * * * * * * * * * * ' TO LISTE-IO-AREA (1:24)
               MOVE '* * * * * * * * * * * * ' TO LISTE-IO-AREA (25:24)
               MOVE '* * * * * * * * * * * * ' TO LISTE-IO-AREA (49:24)
               MOVE '* * * * * * * * * * * * ' TO LISTE-IO-AREA (73:24)
               MOVE '* * * * * * * * * * * * ' TO LISTE-IO-AREA (97:24)
               MOVE '* * * * * * '         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE KUNDE                  TO LISTE-IO-AREA (1:6)
               MOVE NAVN1                  TO LISTE-IO-AREA (9:30)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ADR                    TO LISTE-IO-AREA (9:30)
               MOVE POSTNR                 TO LISTE-IO-AREA (40:4)
               MOVE POSTST                 TO LISTE-IO-AREA (45:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE VGR                    TO LISTE-IO-AREA (3:5)
               MOVE VGRNA                  TO LISTE-IO-AREA (9:32)
               MOVE ANTDM                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (42:10)
               MOVE ANTAK                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (52:10)
               MOVE ANTAKF                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (62:10)
               MOVE BELDM                  TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (74:11)
               MOVE BELDF                  TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (86:11)
               MOVE BELAK                  TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (98:11)
               MOVE BELAKF                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (110:11)
               MOVE BELAV                  TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (122:11)
      ******************************************************
      * TOTALSUMMER PR. KUNDE.                             *
      ******************************************************
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-37)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE KUNVG1                 TO LISTE-IO-AREA (3:1)
               MOVE 'S U B T O T A L'      TO LISTE-IO-AREA (9:15)
               MOVE SKADM                  TO EDIT-SKADM
               MOVE EDIT-SKADM             TO LISTE-IO-AREA (42:10)
               INITIALIZE SKADM
               MOVE SKAAK                  TO EDIT-SKAAK
               MOVE EDIT-SKAAK             TO LISTE-IO-AREA (52:10)
               INITIALIZE SKAAK
               MOVE SKAAKF                 TO EDIT-SKAAKF
               MOVE EDIT-SKAAKF            TO LISTE-IO-AREA (62:10)
               INITIALIZE SKAAKF
               MOVE SKBDM                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (73:12)
               INITIALIZE SKBDM
               MOVE SKBDMF                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (85:12)
               INITIALIZE SKBDMF
               MOVE SKBAK                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (97:12)
               INITIALIZE SKBAK
               MOVE SKBAKF                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (109:12)
               INITIALIZE SKBAKF
               MOVE SKBAV                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (121:12)
               INITIALIZE SKBAV
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-31)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE KUNDE                  TO LISTE-IO-AREA (1:6)
               MOVE 'K U N D E T O T A L'  TO LISTE-IO-AREA (9:19)
               MOVE L1ADM                  TO EDIT-L1ADM
               MOVE EDIT-L1ADM             TO LISTE-IO-AREA (42:10)
               INITIALIZE L1ADM
               MOVE L1AAK                  TO EDIT-L1AAK
               MOVE EDIT-L1AAK             TO LISTE-IO-AREA (52:10)
               INITIALIZE L1AAK
               MOVE L1AAKF                 TO EDIT-L1AAKF
               MOVE EDIT-L1AAKF            TO LISTE-IO-AREA (62:10)
               INITIALIZE L1AAKF
               MOVE L1BDM                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (73:12)
               INITIALIZE L1BDM
               MOVE L1BDMF                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (85:12)
               INITIALIZE L1BDMF
               MOVE L1BAK                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (97:12)
               INITIALIZE L1BAK
               MOVE L1BAKF                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (109:12)
               INITIALIZE L1BAKF
               MOVE L1BAV                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (121:12)
               INITIALIZE L1BAV
      ******************************************************
      * TOTALSUMMER PR. HANDELSDISTRIKT.                   *
      ******************************************************
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-32)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '***  SALG I ANTALL OG' TO LISTE-IO-AREA (33:21)
               MOVE 'BELØP PR KUNDE OG'    TO LISTE-IO-AREA (55:17)
               MOVE 'SELGER      ***'      TO LISTE-IO-AREA (73:15)
               MOVE MNDNA                  TO LISTE-IO-AREA (91:9)
               MOVE AAR                    TO LISTE-IO-AREA (101:4)
               MOVE 'OPPGNR=STA33'         TO LISTE-IO-AREA (107:12)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (120:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (125:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***  SELGER'          TO LISTE-IO-AREA (33:11)
               IF  (NOT-I-10)
                   MOVE SELGER             TO LISTE-IO-AREA (46:30)
               END-IF
               MOVE '***'                  TO LISTE-IO-AREA (85:3)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALER  HND='        TO LISTE-IO-AREA (1:13)
               MOVE HDIST                  TO LISTE-IO-AREA (14:3)
               MOVE 'SELGERNR='            TO LISTE-IO-AREA (19:9)
               MOVE KAT                    TO LISTE-IO-AREA (28:3)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (45:6)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (55:6)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (65:6)
               MOVE 'BELØP'                TO LISTE-IO-AREA (79:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (91:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (103:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (115:5)
               MOVE 'AVVIK'                TO LISTE-IO-AREA (127:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DENNE MND'            TO LISTE-IO-AREA (42:9)
               MOVE 'AKK IÅR'              TO LISTE-IO-AREA (54:7)
               MOVE 'AKK IFJOR'            TO LISTE-IO-AREA (62:9)
               MOVE 'DENNE MND'            TO LISTE-IO-AREA (75:9)
               MOVE 'D.MND IFJOR'          TO LISTE-IO-AREA (85:11)
               MOVE 'AKK IÅR'              TO LISTE-IO-AREA (101:7)
               MOVE 'AKK IFJOR'            TO LISTE-IO-AREA (111:9)
               MOVE 'BELØP'                TO LISTE-IO-AREA (127:5)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-46 AND NOT-I-86)
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (2:1)
               MOVE HVGR                   TO LISTE-IO-AREA (3:5)
               MOVE HVGRNA                 TO LISTE-IO-AREA (9:32)
               MOVE HANTDM                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (42:10)
               MOVE HANTAK                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (52:10)
               MOVE HAAKF                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (62:10)
               MOVE HBELDM                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (74:11)
               MOVE HBELDF                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (86:11)
               MOVE HBELAK                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (98:11)
               MOVE HBAKF                  TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (110:11)
               MOVE HBELAV                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (122:11)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-47)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE HNDVG1                 TO LISTE-IO-AREA (3:1)
               MOVE 'S U B T O T A L'      TO LISTE-IO-AREA (9:15)
               MOVE SHADM                  TO EDIT-SHADM
               MOVE EDIT-SHADM             TO LISTE-IO-AREA (42:10)
               INITIALIZE SHADM
               MOVE SHAAK                  TO EDIT-SHAAK
               MOVE EDIT-SHAAK             TO LISTE-IO-AREA (52:10)
               INITIALIZE SHAAK
               MOVE SHAAKF                 TO EDIT-SHAAKF
               MOVE EDIT-SHAAKF            TO LISTE-IO-AREA (62:10)
               INITIALIZE SHAAKF
               MOVE SHBDM                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (73:12)
               INITIALIZE SHBDM
               MOVE SHBDMF                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (85:12)
               INITIALIZE SHBDMF
               MOVE SHBAK                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (97:12)
               INITIALIZE SHBAK
               MOVE SHBAKF                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (109:12)
               INITIALIZE SHBAKF
               MOVE SHBAV                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (121:12)
               INITIALIZE SHBAV
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-22)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'S U M   D I S T'      TO LISTE-IO-AREA (9:15)
               MOVE HDIST                  TO LISTE-IO-AREA (27:3)
               MOVE L2ADM                  TO EDIT-L2ADM
               MOVE EDIT-L2ADM             TO LISTE-IO-AREA (42:10)
               INITIALIZE L2ADM
               MOVE L2AAK                  TO EDIT-L2AAK
               MOVE EDIT-L2AAK             TO LISTE-IO-AREA (52:10)
               INITIALIZE L2AAK
               MOVE L2AAKF                 TO EDIT-L2AAKF
               MOVE EDIT-L2AAKF            TO LISTE-IO-AREA (62:10)
               INITIALIZE L2AAKF
               MOVE L2BDM                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (73:12)
               INITIALIZE L2BDM
               MOVE L2BDMF                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (85:12)
               INITIALIZE L2BDMF
               MOVE L2BAK                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (97:12)
               INITIALIZE L2BAK
               MOVE L2BAKF                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (109:12)
               INITIALIZE L2BAKF
               MOVE L2BAV                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (121:12)
               INITIALIZE L2BAV
      ******************************************************
      * TOTALSUMMER PR. KUNDEKAT./SELGERNR.                *
      ******************************************************
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-33)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '***  SALG I ANTALL OG' TO LISTE-IO-AREA (33:21)
               MOVE 'BELØP PR KUNDE OG'    TO LISTE-IO-AREA (55:17)
               MOVE 'VAREGRUPPE  ***'      TO LISTE-IO-AREA (73:15)
               MOVE MNDNA                  TO LISTE-IO-AREA (91:9)
               MOVE AAR                    TO LISTE-IO-AREA (101:4)
               MOVE 'OPPGNR=STA33'         TO LISTE-IO-AREA (107:12)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (120:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (125:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***  SELGER'          TO LISTE-IO-AREA (33:11)
               IF  (NOT-I-10)
                   MOVE SELGER             TO LISTE-IO-AREA (46:30)
               END-IF
               MOVE '***'                  TO LISTE-IO-AREA (85:3)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALER SELGERNR     ' TO LISTE-IO-AREA (1:21)
               MOVE KAT                    TO LISTE-IO-AREA (23:3)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (45:6)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (55:6)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (65:6)
               MOVE 'BELØP'                TO LISTE-IO-AREA (79:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (91:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (103:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (115:5)
               MOVE 'AVVIK'                TO LISTE-IO-AREA (127:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DENNE MND'            TO LISTE-IO-AREA (42:9)
               MOVE 'AKK IÅR'              TO LISTE-IO-AREA (54:7)
               MOVE 'AKK IFJOR'            TO LISTE-IO-AREA (62:9)
               MOVE 'DENNE MND'            TO LISTE-IO-AREA (75:9)
               MOVE 'D.MND IFJOR'          TO LISTE-IO-AREA (85:11)
               MOVE 'AKK IÅR'              TO LISTE-IO-AREA (101:7)
               MOVE 'AKK IFJOR'            TO LISTE-IO-AREA (111:9)
               MOVE 'BELØP'                TO LISTE-IO-AREA (127:5)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-56)
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (2:1)
               MOVE KVGR                   TO LISTE-IO-AREA (3:5)
               MOVE KVGRNA                 TO LISTE-IO-AREA (9:32)
               MOVE KANTDM                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (42:10)
               MOVE KANTAK                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (52:10)
               MOVE KAAKF                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (62:10)
               MOVE KBELDM                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (74:11)
               MOVE KBELDF                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (86:11)
               MOVE KBELAK                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (98:11)
               MOVE KBAKF                  TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (110:11)
               MOVE KBELAV                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (122:11)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-23)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SUM SELGERNR='        TO LISTE-IO-AREA (14:13)
               MOVE KAT                    TO LISTE-IO-AREA (27:3)
               MOVE L3ADM                  TO EDIT-L3ADM
               MOVE EDIT-L3ADM             TO LISTE-IO-AREA (42:10)
               INITIALIZE L3ADM
               MOVE L3AAK                  TO EDIT-L3AAK
               MOVE EDIT-L3AAK             TO LISTE-IO-AREA (52:10)
               INITIALIZE L3AAK
               MOVE L3AAKF                 TO EDIT-L3AAKF
               MOVE EDIT-L3AAKF            TO LISTE-IO-AREA (62:10)
               INITIALIZE L3AAKF
               MOVE L3BDM                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (73:12)
               INITIALIZE L3BDM
               MOVE L3BDMF                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (85:12)
               INITIALIZE L3BDMF
               MOVE L3BAK                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (97:12)
               INITIALIZE L3BAK
               MOVE L3BAKF                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (109:12)
               INITIALIZE L3BAKF
               MOVE L3BAV                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (121:12)
               INITIALIZE L3BAV
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-57)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE SELVG1                 TO LISTE-IO-AREA (3:1)
               MOVE 'S U B T O T A L'      TO LISTE-IO-AREA (9:15)
               MOVE L5ADM                  TO EDIT-L5ADM
               MOVE EDIT-L5ADM             TO LISTE-IO-AREA (42:10)
               INITIALIZE L5ADM
               MOVE L5AAK                  TO EDIT-L5AAK
               MOVE EDIT-L5AAK             TO LISTE-IO-AREA (52:10)
               INITIALIZE L5AAK
               MOVE L5AAKF                 TO EDIT-L5AAKF
               MOVE EDIT-L5AAKF            TO LISTE-IO-AREA (62:10)
               INITIALIZE L5AAKF
               MOVE L5BDM                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (73:12)
               INITIALIZE L5BDM
               MOVE L5BDMF                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (85:12)
               INITIALIZE L5BDMF
               MOVE L5BAK                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (97:12)
               INITIALIZE L5BAK
               MOVE L5BAKF                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (109:12)
               INITIALIZE L5BAKF
               MOVE L5BAV                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (121:12)
               INITIALIZE L5BAV
      ******************************************************
      * TOTALSUMMER PR. FIRMA.                             *
      ******************************************************
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-34)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '***  SALG I ANTALL OG' TO LISTE-IO-AREA (33:21)
               MOVE 'BELØP PR KUNDE OG'    TO LISTE-IO-AREA (55:17)
               MOVE 'SELGER      ***'      TO LISTE-IO-AREA (73:15)
               MOVE MNDNA                  TO LISTE-IO-AREA (91:9)
               MOVE AAR                    TO LISTE-IO-AREA (101:4)
               MOVE 'OPPGNR=STA33'         TO LISTE-IO-AREA (107:12)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (120:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (125:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'F I R M A - T O T A L   ' TO LISTE-IO-AREA (1:24)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (45:6)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (55:6)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (65:6)
               MOVE 'BELØP'                TO LISTE-IO-AREA (79:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (91:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (103:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (115:5)
               MOVE 'AVVIK'                TO LISTE-IO-AREA (127:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DENNE MND'            TO LISTE-IO-AREA (42:9)
               MOVE 'AKK IÅR'              TO LISTE-IO-AREA (54:7)
               MOVE 'AKK IFJOR'            TO LISTE-IO-AREA (62:9)
               MOVE 'DENNE MND'            TO LISTE-IO-AREA (75:9)
               MOVE 'D.MND IFJOR'          TO LISTE-IO-AREA (85:11)
               MOVE 'AKK IÅR'              TO LISTE-IO-AREA (101:7)
               MOVE 'AKK IFJOR'            TO LISTE-IO-AREA (111:9)
               MOVE 'BELØP'                TO LISTE-IO-AREA (127:5)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-66)
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (2:1)
               MOVE FVGR                   TO LISTE-IO-AREA (3:5)
               MOVE FVGRNA                 TO LISTE-IO-AREA (9:32)
               MOVE FANTDM                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (42:10)
               MOVE FANTAK                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (52:10)
               MOVE FAAKF                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (62:10)
               MOVE FBELDM                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (74:11)
               MOVE FBELDF                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (86:11)
               MOVE FBELAK                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (98:11)
               MOVE FBAKF                  TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (110:11)
               MOVE FBELAV                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (122:11)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-67)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRVG1                 TO LISTE-IO-AREA (3:1)
               MOVE 'S U B T O T A L'      TO LISTE-IO-AREA (9:15)
               MOVE L6ADM                  TO EDIT-L6ADM
               MOVE EDIT-L6ADM             TO LISTE-IO-AREA (42:10)
               INITIALIZE L6ADM
               MOVE L6AAK                  TO EDIT-L6AAK
               MOVE EDIT-L6AAK             TO LISTE-IO-AREA (52:10)
               INITIALIZE L6AAK
               MOVE L6AAKF                 TO EDIT-L6AAKF
               MOVE EDIT-L6AAKF            TO LISTE-IO-AREA (62:10)
               INITIALIZE L6AAKF
               MOVE L6BDM                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (73:12)
               INITIALIZE L6BDM
               MOVE L6BDMF                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (85:12)
               INITIALIZE L6BDMF
               MOVE L6BAK                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (97:12)
               INITIALIZE L6BAK
               MOVE L6BAKF                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (109:12)
               INITIALIZE L6BAKF
               MOVE L6BAV                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (121:12)
               INITIALIZE L6BAV
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-24)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'F I R M A T O T A L'  TO LISTE-IO-AREA (9:19)
               MOVE L4ADM                  TO EDIT-L4ADM
               MOVE EDIT-L4ADM             TO LISTE-IO-AREA (42:10)
               INITIALIZE L4ADM
               MOVE L4AAK                  TO EDIT-L4AAK
               MOVE EDIT-L4AAK             TO LISTE-IO-AREA (52:10)
               INITIALIZE L4AAK
               MOVE L4AAKF                 TO EDIT-L4AAKF
               MOVE EDIT-L4AAKF            TO LISTE-IO-AREA (62:10)
               INITIALIZE L4AAKF
               MOVE L4BDM                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (73:12)
               INITIALIZE L4BDM
               MOVE L4BDMF                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (85:12)
               INITIALIZE L4BDMF
               MOVE L4BAK                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (97:12)
               INITIALIZE L4BAK
               MOVE L4BAKF                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (109:12)
               INITIALIZE L4BAKF
               MOVE L4BAV                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (121:12)
               INITIALIZE L4BAV
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '***  SALG I ANTALL OG' TO LISTE-IO-AREA (33:21)
               MOVE 'BELØP PR KUNDE OG'    TO LISTE-IO-AREA (55:17)
               MOVE 'SELGER.     ***'      TO LISTE-IO-AREA (73:15)
               MOVE MNDNA                  TO LISTE-IO-AREA (91:9)
               MOVE AAR                    TO LISTE-IO-AREA (101:4)
               MOVE 'OPPGNR=STA33'         TO LISTE-IO-AREA (107:12)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (120:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (125:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***  SELGER'          TO LISTE-IO-AREA (33:11)
               IF  (NOT-I-10)
                   MOVE SELGER             TO LISTE-IO-AREA (46:30)
               END-IF
               MOVE '***'                  TO LISTE-IO-AREA (85:3)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-70)
                   MOVE 'KUNDE   HND='     TO LISTE-IO-AREA (1:12)
               END-IF
               IF  (NOT-I-70)
                   MOVE HDIST              TO LISTE-IO-AREA (13:3)
               END-IF
               IF  (NOT-I-70)
                   MOVE 'SELGERNR='        TO LISTE-IO-AREA (18:9)
               END-IF
               IF  (NOT-I-70)
                   MOVE KAT                TO LISTE-IO-AREA (27:3)
               END-IF
               MOVE 'ANTALL'               TO LISTE-IO-AREA (45:6)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (55:6)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (65:6)
               MOVE 'BELØP'                TO LISTE-IO-AREA (79:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (91:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (103:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (115:5)
               MOVE 'AVVIK'                TO LISTE-IO-AREA (127:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DENNE MND'            TO LISTE-IO-AREA (42:9)
               MOVE 'AKK IÅR'              TO LISTE-IO-AREA (54:7)
               MOVE 'AKK IFJOR'            TO LISTE-IO-AREA (62:9)
               MOVE 'DENNE MND'            TO LISTE-IO-AREA (75:9)
               MOVE 'D.MND IFJOR'          TO LISTE-IO-AREA (85:11)
               MOVE 'AKK IÅR'              TO LISTE-IO-AREA (101:7)
               MOVE 'AKK IFJOR'            TO LISTE-IO-AREA (111:9)
               MOVE 'BELØP'                TO LISTE-IO-AREA (127:5)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-86 AND NOT-I-70)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '***  SALG I ANTALL OG' TO LISTE-IO-AREA (33:21)
               MOVE 'BELØP PR KUNDE OG'    TO LISTE-IO-AREA (55:17)
               MOVE 'SELGER.     ***'      TO LISTE-IO-AREA (73:15)
               MOVE MNDNA                  TO LISTE-IO-AREA (91:9)
               MOVE AAR                    TO LISTE-IO-AREA (101:4)
               MOVE 'OPPGNR=STA33'         TO LISTE-IO-AREA (107:12)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (120:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (125:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***  SELGER'          TO LISTE-IO-AREA (33:11)
               IF  (NOT-I-10)
                   MOVE SELGER             TO LISTE-IO-AREA (46:30)
               END-IF
               MOVE '***'                  TO LISTE-IO-AREA (85:3)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-70)
                   MOVE 'KUNDE   HND='     TO LISTE-IO-AREA (1:12)
               END-IF
               IF  (NOT-I-70)
                   MOVE HDIST              TO LISTE-IO-AREA (13:3)
               END-IF
               IF  (NOT-I-70)
                   MOVE 'SELGERNR='        TO LISTE-IO-AREA (18:9)
               END-IF
               IF  (NOT-I-70)
                   MOVE KAT                TO LISTE-IO-AREA (27:3)
               END-IF
               MOVE 'ANTALL'               TO LISTE-IO-AREA (45:6)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (55:6)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (65:6)
               MOVE 'BELØP'                TO LISTE-IO-AREA (79:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (91:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (103:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (115:5)
               MOVE 'AVVIK'                TO LISTE-IO-AREA (127:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DENNE MND'            TO LISTE-IO-AREA (42:9)
               MOVE 'AKK IÅR'              TO LISTE-IO-AREA (54:7)
               MOVE 'AKK IFJOR'            TO LISTE-IO-AREA (62:9)
               MOVE 'DENNE MND'            TO LISTE-IO-AREA (75:9)
               MOVE 'D.MND IFJOR'          TO LISTE-IO-AREA (85:11)
               MOVE 'AKK IÅR'              TO LISTE-IO-AREA (101:7)
               MOVE 'AKK IFJOR'            TO LISTE-IO-AREA (111:9)
               MOVE 'BELØP'                TO LISTE-IO-AREA (127:5)
               MOVE 2                      TO LISTE-AFTER-SPACE
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
           INITIALIZE PARFILE-DATA-FIELDS
           SET PARFILE-EOF-OFF             TO TRUE
           SET PARFILE-PROCESS             TO TRUE
           OPEN INPUT PARFILE
           SET PRTFILE-LEVEL-INIT          TO TRUE
           INITIALIZE PRTFILE-DATA-FIELDS
           SET PRTFILE-EOF-OFF             TO TRUE
           SET PRTFILE-PROCESS             TO TRUE
           OPEN INPUT PRTFILE
           INITIALIZE STATTAB-DATA-FIELDS
           OPEN INPUT STATTAB
           INITIALIZE PRTHND-DATA-FIELDS
           OPEN INPUT PRTHND
           INITIALIZE PRTKAT-DATA-FIELDS
           OPEN INPUT PRTKAT
           INITIALIZE PRTFIR-DATA-FIELDS
           OPEN INPUT PRTFIR
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARFILE
           CLOSE PRTFILE
           CLOSE STATTAB
           CLOSE PRTHND
           CLOSE PRTKAT
           CLOSE PRTFIR
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
