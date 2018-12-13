       IDENTIFICATION DIVISION.
       PROGRAM-ID.      STDLIC.
       AUTHOR.              JN.
      **
      **  Sidst ændret af MS : Fre 25 Jan 2013 08:30:37 CET
      **  Company............: WeDo Computer Systems ApS
      **  Product............: Standard-Program
      **  Release............: 7.2
      **  Computer...........: Any UNIX-box
      **  Program............: Licens-/Adgangs-kontrol.
      **
      **  Programmør.........: John Niclasen
      **
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           COPY     "csstd.cpy".
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY     "sepasswd.cpy".
           COPY     "seprof01.cpy".
           COPY     "seprof02.cpy".
           COPY     "sestdf12.cpy".
           COPY     "sestdf14.cpy".
           COPY     "sestdf15.cpy".
           COPY     "sestdf16.cpy".
           COPY     "sestdf17.cpy".
           COPY     "sestdf19.cpy".
           COPY     "sestdf96.cpy".
           COPY     "sestdf97.cpy".
       DATA DIVISION.
       FILE SECTION.
           COPY     "fdpasswd.cpy".
           COPY     "fdprof01.cpy".
           COPY     "fdprof02.cpy".
           COPY     "fdstdf12.cpy".
           COPY     "fdstdf14.cpy".
           COPY     "fdstdf15.cpy".
           COPY     "fdstdf16.cpy".
           COPY     "fdstdf17.cpy".
           COPY     "fdstdf19.cpy".
           COPY     "fdstdf96.cpy".
           COPY     "fdstdf97.cpy".
       WORKING-STORAGE SECTION.
           COPY     "wsstd.cpy".
           COPY     "wsdato.cpy".
           COPY     "wssystem.cpy".
           COPY     "wspasswd.cpy".
           COPY     "wsprof01.cpy".
           COPY     "wsprof02.cpy".
           COPY     "wsstdf12.cpy".
           COPY     "wsstdf14.cpy".
           COPY     "wsstdf15.cpy".
           COPY     "wsstdf16.cpy".
           COPY     "wsstdf17.cpy".
           COPY     "wsstdf19.cpy".
           COPY     "wsstdf96.cpy".
           COPY     "wsstdf97.cpy".
       77  W-EX-MODUL          PIC XXX.
       77  W-GEM-AFSNIT        PIC 9.
       77  W-GEM-FELTNR        PIC 999.
       77  W-GEM-DATO          PIC 9(6).
       77  W-GEM-TID           PIC 9(8).
       77  W-DESKTOP-DIR       PIC X(128).
       77  W-MYDIR             USAGE HANDLE.
      
       77  W-AAMMDD            PIC 9(6).
       01  W-AAMMDD-X REDEFINES W-AAMMDD.
           03 W-AAMMDD-AA      PIC 99.
           03 W-AAMMDD-MM      PIC 99.
           03 W-AAMMDD-DD      PIC 99.
       77  W-DDMMAA            PIC 9(6).
       01  W-DDMMAA-X REDEFINES W-DDMMAA.
           03 W-DDMMAA-DD      PIC 99.
           03 W-DDMMAA-MM      PIC 99.
           03 W-DDMMAA-AA      PIC 99.
       77  W-TID               PIC 9(8).
       01  W-TID-X REDEFINES W-TID.
           03 W-TTMM           PIC 9(4).
           03 FILLER           PIC X(4).
      
       01  W-PASSWORD.
           03 W-PASSWORD1      PIC X(8) COMP-X.
           03 W-PASSWORD2      PIC X(8) COMP-X.
      
       77  W-CC-DAGS-DATO      PIC 9(8).
       77  W-ST15-DATO-SLUT    PIC 9(6).
       77  W-CC-ST15-DATO-SLUT PIC 9(8).
      
       LINKAGE SECTION.
           COPY     "lsstdlic.cpy".
       SCREEN SECTION.
           COPY     "scstdlic.cpy".
       PROCEDURE DIVISION USING LS-STDLIC.
       DECLARATIVES.
           COPY     "uspasswd.cpy".
           COPY     "usprof01.cpy".
           COPY     "usprof02.cpy".
           COPY     "usstdf12.cpy".
           COPY     "usstdf14.cpy".
      *    COPY     "usstdf15.cpy".
      *
       USE-STDF15 SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON STDF15.
       ST15-1.
           IF       FILE-CONFLICT
                    CALL     "stdt02"
                    PERFORM  OPEN-IO-ST15
           ELSE
                    MOVE     "stdfil15" TO US-FILNAVN
                    PERFORM  USE-ERROR.
       ST15-9.
           EXIT.
           COPY     "usstdf16.cpy".
           COPY     "usstdf17.cpy".
           COPY     "usstdf19.cpy".
           COPY     "usstdf96.cpy".
           COPY     "usstdf97.cpy".
       END DECLARATIVES.
      ********************************
       MAIN-LOGIC SECTION.
      ********************************
       ML-CONTROL.
           PERFORM  P-OPSTART.
           PERFORM  R-OPEN.
      
      *    ACCEPT   TERMINAL-ABILITIES FROM TERMINAL-INFO.
      
           CALL     "C$TOUPPER" USING LS-STDLIC-PROGRAM VALUE 6.
      
           IF       LS-STDLIC-FUNK = 1
                    PERFORM  M1-INIT-CALL
           ELSE IF  LS-STDLIC-FUNK = 2
                    PERFORM  M2-EXIT-CALL
           ELSE IF  LS-STDLIC-FUNK = 3
                    PERFORM  M3-INIT-STD000
           ELSE IF  LS-STDLIC-FUNK = 4
                    PERFORM  M4-STOP-RUN
           ELSE IF  LS-STDLIC-FUNK = 5
                    PERFORM  M5-CHK-ME
           ELSE IF  LS-STDLIC-FUNK = 6
                    PERFORM  M6-ADG-TYPE.
      
           PERFORM  R-CLOSE.
           PERFORM  P-AFSLUT.
      
       ML-EXIT.
           EXIT     PROGRAM.
      
      ***********************
       M1-HELP SECTION.
      ***********************
       M1-INIT-CALL.
      * Check adgangskontrol.
           PERFORM  M1-CHK-ADGANG.
      
      * Check om licenser er overskredet.
           IF       LS-STDLIC-VAL-CALL
                AND LS-STDLIC-MODUL NOT = W-EX-MODUL
                    PERFORM  M1-CHK-LICENS.
      
      *    IF       LS-STDLIC-VAL-CALL
      *         AND ST17-ADGANGSNIVEAU NOT = "0"
      *             MOVE     ST17-ADGANGSNIVEAU TO EX-ADGANGSNIVEAU.
      
       M1-CHK-ADGANG.
           MOVE     LS-STDLIC-MODUL TO ST17-MODUL.
           MOVE     LS-STDLIC-PRGM  TO ST17-PRGM.
           MOVE     SPACE           TO ST17-MED.
           MOVE     ZERO            TO ST17-GRP.
           PERFORM  START-NEXT-ST17-NL.
      
           IF       VAL-ST17
                AND ST17-MODUL = LS-STDLIC-MODUL
                AND ST17-PRGM  = LS-STDLIC-PRGM
                    PERFORM  M1-CHK-MED
           ELSE
                    MOVE     LS-STDLIC-MODUL TO ST17-MODUL
                    MOVE     SPACE           TO ST17-PRGM
                                                ST17-MED
                    MOVE     ZERO            TO ST17-GRP
                    PERFORM  START-NEXT-ST17-NL
                    IF       VAL-ST17
                         AND ST17-MODUL = LS-STDLIC-MODUL
                         AND ST17-PRGM  = SPACE
                             PERFORM  M1-CHK-MED
                    ELSE
                             SET      LS-STDLIC-VAL-CALL TO TRUE.
      
       M1-CHK-MED.
           MOVE     EX-MED TO ST17-MED.
           MOVE     ZERO   TO ST17-GRP.
           PERFORM  READ-ST17-NL.
      
           IF       VAL-ST17
                    IF       ST17-PASSWORD-FLG = 1
                             PERFORM  M1-CHK-ADGANGSKONTROL
                    ELSE
                             SET      LS-STDLIC-VAL-CALL TO TRUE
                    END-IF
                    IF       LS-STDLIC-VAL-CALL
                         AND ST17-ADGANGSNIVEAU NOT = "0"
                             MOVE     ST17-ADGANGSNIVEAU
                                   TO EX-ADGANGSNIVEAU
                    END-IF
           ELSE
                    PERFORM  M1-CHK-GRP.
      
       M1-CHK-GRP.
           SET      KEY2-ST97 TO TRUE.
           MOVE     EX-MED TO ST97-MED2.
           MOVE     ZERO   TO ST97-GRP2.
           PERFORM  START-NEXT-ST97-NL.
      
           IF       VAL-ST97
                AND ST97-MED2 = EX-MED
                    MOVE     SPACE    TO ST17-MED
                    MOVE     ST97-GRP TO ST17-GRP
                    PERFORM  READ-ST17-NL
                    PERFORM  M1-NEXT-GRP
                             UNTIL ST97-MED2 NOT = EX-MED
                                OR INV-ST97
                                OR VAL-ST17.
      
           IF       VAL-ST97
                AND ST97-MED2 = EX-MED
                AND VAL-ST17
                    IF       ST17-PASSWORD-FLG = 1
                             PERFORM  M1-CHK-ADGANGSKONTROL
                    ELSE
                             SET      LS-STDLIC-VAL-CALL TO TRUE
                    END-IF
                    IF       LS-STDLIC-VAL-CALL
                             PERFORM  M1-ADGANGSNIVEAU-GRP
                    END-IF
           ELSE
                    MOVE     SPACE TO ST17-MED
                    MOVE     ZERO  TO ST17-GRP
                    PERFORM  READ-ST17-NL
                    IF       VAL-ST17
                             IF       ST17-PASSWORD-FLG = 1
                                      PERFORM  M1-CHK-ADGANGSKONTROL
                             ELSE
                                      SET      LS-STDLIC-VAL-CALL
                                            TO TRUE
                             END-IF
                             IF       LS-STDLIC-VAL-CALL
                                      PERFORM  M1-ADGANGSNIVEAU-GRP
                             END-IF
                    ELSE
                             PERFORM  P-OPEN-WINDOW2
                             DISPLAY  STDLIC-6
                             MOVE     SPACE TO WS-OK
                             DISPLAY  STDLIC-6-INPUT
                             MOVE     8    TO EX-AFSNIT
                             MOVE     ZERO TO EX-FELTNR
                             ACCEPT   STDLIC-6-INPUT
                             PERFORM  P-CLOSE-WINDOW
                             SET      LS-STDLIC-INV-CALL TO TRUE.
      
       M1-NEXT-GRP.
           PERFORM  READ-NEXT-ST97-NL.
      
           IF       VAL-ST97
                AND ST97-MED2 = EX-MED
                    MOVE     SPACE    TO ST17-MED
                    MOVE     ST97-GRP TO ST17-GRP
                    PERFORM  READ-ST17-NL.
      
       M1-ADGANGSNIVEAU-GRP.
           IF       ST17-ADGANGSNIVEAU = "0"
                    IF       ST97-ADGANGSNIVEAU = "0"
                             MOVE     ST17-GRP TO ST96-GRP
                             PERFORM  READ-ST96-NL
                             MOVE     ST96-ADGANGSNIVEAU
                                   TO EX-ADGANGSNIVEAU
                    ELSE
                             MOVE     ST97-ADGANGSNIVEAU
                                   TO EX-ADGANGSNIVEAU
           ELSE
                    MOVE     ST17-ADGANGSNIVEAU TO EX-ADGANGSNIVEAU.
      
       M1-CHK-ADGANGSKONTROL.
           PERFORM  R-OPEN-ST14.
           MOVE     ZERO  TO ST14-LICENS.
           MOVE     SPACE TO ST14-PRODUKT.
           PERFORM  START-NEXT-ST14.
      
           IF       VAL-ST14
                AND ST14-ADGANGSKONTROL = 1
                    PERFORM  M1-CHK-PASSWORD.
      
           PERFORM  R-CLOSE-ST14.
      
       M1-CHK-PASSWORD.
           PERFORM  P-OPEN-WINDOW2.
           DISPLAY  STDLIC-5.
           MOVE     SPACE TO W-PASSWORD.
           DISPLAY  STDLIC-5-INPUT.
           MOVE     7    TO EX-AFSNIT.
           MOVE     ZERO TO EX-FELTNR.
           ACCEPT   STDLIC-5-INPUT.
           PERFORM  CHK-KEY.
      
           IF       KEY-UP OR KEY-ESC
                    SET      LS-STDLIC-INV-CALL TO TRUE
           ELSE
                    IF       ST14-KRYPTEREDE-PASSWORD = 1
                             PERFORM  M1-CHK-PASSWORD2
                    ELSE
                             PERFORM  M1-CHK-PASSWORD3.
      
           PERFORM  P-CLOSE-WINDOW.
      
       M1-CHK-PASSWORD2.
           MOVE     W-PASSWORD TO WS-INPUT-STRING.
           PERFORM  KRYPTER-PASSWORD.
      
           IF       PSWD-OK
                    PERFORM  M1-CHK-PASSWORD3
           ELSE
                    SET      LS-STDLIC-INV-CALL TO TRUE.
      
       M1-CHK-PASSWORD3.
           PERFORM  INIT-ST12.
           PERFORM  R-OPEN-ST12.
           MOVE     EX-MED TO ST12-MED.
           PERFORM  READ-ST12-NL.
           PERFORM  R-CLOSE-ST12.
      
      *    ELSE IF  W-PASSWORD = ST17-PASSWORD
           IF      (ST14-KRYPTEREDE-PASSWORD = 1
                AND PSWD-PASSWORD1 = ST12-PASSWORD1
                AND PSWD-PASSWORD2 = ST12-PASSWORD2
                AND PSWD-PASSWORD3 = ST12-PASSWORD3)
                OR (ST14-KRYPTEREDE-PASSWORD = ZERO
                AND W-PASSWORD1 = ST12-PASSWORD1
                AND W-PASSWORD2 = ST12-PASSWORD2)
                    SET      LS-STDLIC-VAL-CALL TO TRUE
           ELSE
                    DISPLAY  STDLIC-6
                    MOVE     SPACE TO WS-OK
                    DISPLAY  STDLIC-6-INPUT
                    MOVE     8    TO EX-AFSNIT
                    MOVE     ZERO TO EX-FELTNR
                    ACCEPT   STDLIC-6-INPUT
                    SET      LS-STDLIC-INV-CALL TO TRUE.
      
       M1-CHK-LICENS.
           MOVE     LS-STDLIC-MODUL TO ST15-MODUL.
           PERFORM  READ-ST15.
      
           IF       VAL-ST15
                AND ST15-MODUL-LINK NOT = SPACE
                    MOVE     ST15-MODUL-LINK TO ST15-MODUL
                    PERFORM  READ-ST15.
      
           IF       VAL-ST15
                AND ST15-MODUL NOT = "STD"
                    ACCEPT   WS-AAMMDD FROM DATE
                    PERFORM  DAN-AAAA
                    MOVE     WS-AAAAMMDD TO W-CC-DAGS-DATO
                    MOVE     ST15-DATO-SLUT TO WS-AAMMDD
                    PERFORM  DAN-AAAA
                    MOVE     WS-AAAAMMDD TO W-CC-ST15-DATO-SLUT
                    IF       W-CC-DAGS-DATO > W-CC-ST15-DATO-SLUT
                             PERFORM  UNLOCK-ST15
                             PERFORM  P-OPEN-WINDOW3
                             DISPLAY  STDLIC-8
                             MOVE     SPACE TO WS-OK
                             MOVE     ST15-DATO-SLUT TO WS-AAMMDD
                             PERFORM  VEND-AAMMDD
                             MOVE     WS-DDMMAA TO W-ST15-DATO-SLUT
                             DISPLAY  STDLIC-8-INPUT
                             MOVE     8    TO EX-AFSNIT
                             MOVE     ZERO TO EX-FELTNR
                             ACCEPT   STDLIC-8-INPUT
                             PERFORM  P-CLOSE-WINDOW
                             SET      LS-STDLIC-INV-CALL TO TRUE
                    ELSE IF  EX-ADGANGSNIVEAU = "9"
                             PERFORM  M1-OPDAT-BRUGER
                             SET      LS-STDLIC-VAL-CALL TO TRUE
                    ELSE IF  ST15-ANTAL-BRUGT < ST15-ANTAL-BRUGERE
                             ADD      1 TO ST15-ANTAL-BRUGT
                             PERFORM  REWRITE-ST15
                             PERFORM  M1-OPDAT-BRUGER
                             SET      LS-STDLIC-VAL-CALL TO TRUE
                    ELSE
                             PERFORM  UNLOCK-ST15
                             PERFORM  P-OPEN-WINDOW1
                             IF       ST15-ANTAL-BRUGERE = ZERO
                                      DISPLAY  STDLIC-2A
                             ELSE
                                      DISPLAY  STDLIC-2B
                             END-IF
                             SET      NEJ TO TRUE
                             DISPLAY  STDLIC-2-INPUT
                             MOVE     3    TO EX-AFSNIT
                             MOVE     ZERO TO EX-FELTNR
                             ACCEPT   STDLIC-2-INPUT
                             PERFORM  P-CLOSE-WINDOW
                             IF       JA
                                      GO       M1-CHK-LICENS
                             ELSE
                                      SET      LS-STDLIC-INV-CALL
                                               TO TRUE
                             END-IF
                    END-IF
                    END-IF
           ELSE
                    IF       VAL-ST15
                             PERFORM  UNLOCK-ST15
                             SET      LS-STDLIC-VAL-CALL TO TRUE
                    ELSE
                             SET      LS-STDLIC-INV-CALL TO TRUE
                    END-IF
           END-IF.
      
       M1-OPDAT-BRUGER.
           MOVE     EX-MED     TO ST16-MED.
           MOVE     EX-DATO    TO ST16-DATO.
           MOVE     EX-TID     TO ST16-TID.
           MOVE     ST15-MODUL TO ST16-MODUL.
           PERFORM  READ-ST16.
      
           IF       VAL-ST16
                    ADD      1 TO ST16-ANTAL
                    PERFORM  REWRITE-ST16
           ELSE
                    MOVE     1 TO ST16-ANTAL
                    PERFORM  WRITE-ST16.
      
       M1-HELP-EXIT.
           EXIT.
      
      ***********************
       M2-HELP SECTION.
      ***********************
       M2-EXIT-CALL.
           MOVE     LS-STDLIC-MODUL TO ST15-MODUL.
           PERFORM  READ-ST15.
      
           IF       VAL-ST15
                AND ST15-MODUL-LINK NOT = SPACE
                    MOVE     ST15-MODUL-LINK TO ST15-MODUL
                    PERFORM  READ-ST15.
      
           IF       VAL-ST15
                    MOVE     EX-MED     TO ST16-MED
                    MOVE     EX-DATO    TO ST16-DATO
                    MOVE     EX-TID     TO ST16-TID
                    MOVE     ST15-MODUL TO ST16-MODUL
                    PERFORM  READ-ST16
                    IF       VAL-ST16
                         AND ST16-MODUL NOT = "STD"
                             IF       ST16-ANTAL > 1
                                      SUBTRACT 1 FROM ST16-ANTAL
                                      PERFORM  REWRITE-ST16
                             ELSE
                                      PERFORM  DELETE-ST16
                             END-IF
                             IF       ST15-ANTAL-BRUGT > ZERO
                                  AND EX-ADGANGSNIVEAU NOT = "9"
                                      SUBTRACT 1 FROM ST15-ANTAL-BRUGT
                                      PERFORM  REWRITE-ST15
                             ELSE
                                      PERFORM  UNLOCK-ST15
                             END-IF
                             SET      LS-STDLIC-VAL-CALL TO TRUE
                    ELSE
                             PERFORM  UNLOCK-ST15
                             IF       VAL-ST16
                                      PERFORM  UNLOCK-ST16
                                      SET      LS-STDLIC-VAL-CALL
                                               TO TRUE
                             ELSE
                                      SET      LS-STDLIC-INV-CALL
                                               TO TRUE
           ELSE
                    SET      LS-STDLIC-INV-CALL TO TRUE.
      
       M2-HELP-EXIT.
           EXIT.
      
      ***********************
       M3-HELP SECTION.
      ***********************
       M3-INIT-STD000.
           ACCEPT   EX-DATO FROM DATE.
           ACCEPT   EX-TID  FROM TIME.
      
           SET      LS-STDLIC-VAL-CALL TO TRUE.
      
      * For denne bruger slettes login, som er ældre end dags dato.
           MOVE     EX-MED TO ST16-MED.
           MOVE     ZERO   TO ST16-DATO
                              ST16-TID.
           MOVE     SPACE  TO ST16-MODUL.
           PERFORM  START-NEXT-ST16-NL.
      
           PERFORM  M3-SLET-LOGIN UNTIL
                    ST16-DATO     = EX-DATO
                 OR ST16-MED  NOT = EX-MED
                 OR INV-ST16.
      
      * Check om bruger er logget på i forvejen.
           MOVE     EX-MED TO ST16-MED.
           MOVE     ZERO   TO ST16-DATO
                              ST16-TID.
           MOVE     SPACE  TO ST16-MODUL.
           PERFORM  START-NEXT-ST16-NL.
      
           IF       VAL-ST16
                AND ST16-MED = EX-MED
                    PERFORM  M3-BRUGER-FINDES.
      
      * Check om licenser er overskredet.
           IF       LS-STDLIC-VAL-CALL
      * Check om ny licens-fil.
                    PERFORM  M3-CHK-ST14
                    PERFORM  M3-CHK-LOGIN.
      
      * Tjek om div. C:\wedo mapper findes
      *    PERFORM  M3-TJEK-WEDO-DIR.
      
       M3-SLET-LOGIN.
           MOVE     ST16-DATO TO W-GEM-DATO.
           MOVE     ST16-TID  TO W-GEM-TID.
      
           MOVE     SPACE TO ST16-MODUL.
           PERFORM  START-NEXT-ST16.
      
           PERFORM  M3-SLET-MODULER UNTIL
                    ST16-TID  NOT = W-GEM-TID
                 OR ST16-DATO NOT = W-GEM-DATO
                 OR ST16-MED  NOT = EX-MED
                 OR INV-ST16.
      
           IF       EX-ADGANGSNIVEAU NOT = "9"
                    PERFORM  M4-LOGOUT.
      
           MOVE     EX-MED     TO ST16-MED.
           MOVE     W-GEM-DATO TO ST16-DATO.
           MOVE     W-GEM-TID  TO ST16-TID.
           MOVE     "STD"      TO ST16-MODUL.
           PERFORM  READ-ST16.
      
           IF       VAL-ST16
                    PERFORM  DELETE-ST16.
      
           MOVE     EX-MED TO ST16-MED.
           MOVE     ZERO   TO ST16-DATO
                              ST16-TID.
           MOVE     SPACE  TO ST16-MODUL.
           PERFORM  START-NEXT-ST16-NL.
      
       M3-BRUGER-FINDES.
           PERFORM  P-OPEN-WINDOW1.
           DISPLAY  STDLIC-1.
           MOVE     ST16-DATO   TO W-AAMMDD.
           MOVE     W-AAMMDD-AA TO W-DDMMAA-AA.
           MOVE     W-AAMMDD-MM TO W-DDMMAA-MM.
           MOVE     W-AAMMDD-DD TO W-DDMMAA-DD.
           MOVE     ST16-TID    TO W-TID.
           PERFORM  P-CLEAR-F-TASTER.
           SET      NEJ TO TRUE.
           DISPLAY  STDLIC-1-INPUT.
           MOVE     2    TO EX-AFSNIT.
           MOVE     ZERO TO EX-FELTNR.
           ACCEPT   STDLIC-1-INPUT.
           PERFORM  CHK-KEY.
      
           IF       KEY-UP OR KEY-ESC
                    SET      LS-STDLIC-INV-CALL TO TRUE
           ELSE IF  NEJ
                    MOVE     SPACE TO ST16-MODUL
                    PERFORM  START-NEXT-ST16
                    PERFORM  M3-SLET-MODULER
                             UNTIL ST16-MED NOT = EX-MED
                                OR INV-ST16
                    MOVE     EX-MED TO ST16-MED
                    MOVE     ZERO   TO ST16-DATO
                                       ST16-TID
                    MOVE     SPACE  TO ST16-MODUL
                    PERFORM  START-NEXT-ST16
                    PERFORM  M3-SLET-STD
                             UNTIL ST16-MED NOT = EX-MED
                                OR INV-ST16.
      
           PERFORM  P-SET-F-TASTER.
           PERFORM  P-CLOSE-WINDOW.
      
       M3-SLET-MODULER.
           IF       ST16-MODUL NOT = "STD"
                    PERFORM  M3-SLET-MODUL.
      
           PERFORM  READ-NEXT-ST16.
      
       M3-SLET-MODUL.
           IF       EX-ADGANGSNIVEAU NOT = "9"
                    PERFORM  M3-CHECK-ST15.
      
           PERFORM  DELETE-ST16.
      
       M3-CHECK-ST15.
           MOVE     ST16-MODUL TO ST15-MODUL.
           PERFORM  READ-ST15.
      
           IF       VAL-ST15
                    IF       ST16-ANTAL > ST15-ANTAL-BRUGT
                             MOVE     ZERO         TO ST15-ANTAL-BRUGT
                    ELSE
                             SUBTRACT ST16-ANTAL FROM ST15-ANTAL-BRUGT
                    END-IF
                    PERFORM  REWRITE-ST15.
      
       M3-SLET-STD.
           IF       ST16-MODUL = "STD"
                    MOVE     ST16-DATO TO W-AAMMDD
                    MOVE     ST16-TID  TO W-TID
                    IF       EX-ADGANGSNIVEAU NOT = "9"
                             PERFORM  M4-LOGOUT
                    END-IF
                    MOVE     EX-MED    TO ST16-MED
                    MOVE     W-AAMMDD  TO ST16-DATO
                    MOVE     W-TID     TO ST16-TID
                    MOVE     "STD"     TO ST16-MODUL
                    PERFORM  READ-ST16
                    IF       VAL-ST16
                             PERFORM  DELETE-ST16.
      
           PERFORM  READ-NEXT-ST16.
      
       M3-CHK-LOGIN.
           IF       EX-ADGANGSNIVEAU = "9"
                    PERFORM  M3-LOGIN-LEVERANDOER
           ELSE
                    PERFORM  M3-LOGIN.
      
       M3-LOGIN-LEVERANDOER.
           MOVE     LS-STDLIC-MODUL TO ST15-MODUL.
           PERFORM  READ-ST15.
      
           MOVE     EX-MED TO ST16-MED.
           MOVE     ZERO   TO ST16-DATO
                              ST16-TID.
           MOVE     SPACE  TO ST16-MODUL.
           PERFORM  START-NEXT-ST16-NL.
      
           PERFORM  M1-OPDAT-BRUGER.
           SET      LS-STDLIC-VAL-CALL TO TRUE.
      
       M3-LOGIN.
           MOVE     LS-STDLIC-MODUL TO ST15-MODUL.
           PERFORM  READ-ST15.
      
      
      * FD - processer og bruger er nu det samme!!!
      * 2-tallet er den gamle metode, skal blot udkommenteres
      * så er det sokm i gl. dage
      *    IF       ((2 * ST15-ANTAL-BRUGERE) - ST15-ANTAL-PROCESSER)
           IF       ((1 * ST15-ANTAL-BRUGERE) - ST15-ANTAL-PROCESSER)
                    IS POSITIVE
                    ADD      1 TO ST15-ANTAL-PROCESSER
                    MOVE     EX-MED TO ST16-MED
                    MOVE     ZERO   TO ST16-DATO
                                       ST16-TID
                    MOVE     SPACE  TO ST16-MODUL
                    PERFORM  START-NEXT-ST16-NL
                    IF       INV-ST16
                          OR ST16-MED NOT = EX-MED
                             IF       ST15-ANTAL-BRUGT
                                    < ST15-ANTAL-BRUGERE
                                      ADD      1 TO ST15-ANTAL-BRUGT
                                      PERFORM  REWRITE-ST15
                                      PERFORM  M1-OPDAT-BRUGER
                                      SET      LS-STDLIC-VAL-CALL
                                               TO TRUE
                             ELSE
                                      PERFORM  UNLOCK-ST15
                                      PERFORM  P-OPEN-WINDOW1
                                      IF       ST15-ANTAL-BRUGERE = ZERO
                                               DISPLAY  STDLIC-2A
                                      ELSE
                                               DISPLAY  STDLIC-2B
                                      END-IF
                                      PERFORM  P-CLEAR-F-TASTER
                                      SET      NEJ TO TRUE
                                      DISPLAY  STDLIC-2-INPUT
                                      MOVE     3    TO EX-AFSNIT
                                      MOVE     ZERO TO EX-FELTNR
                                      ACCEPT   STDLIC-2-INPUT
                                      PERFORM  P-SET-F-TASTER
                                      PERFORM  P-CLOSE-WINDOW
                                      IF       JA
                                               GO       M3-LOGIN
                                      ELSE
                                               SET LS-STDLIC-INV-CALL
                                                   TO TRUE
                    ELSE
                             PERFORM  REWRITE-ST15
                             PERFORM  M1-OPDAT-BRUGER
                             SET      LS-STDLIC-VAL-CALL TO TRUE
           ELSE
                    PERFORM  UNLOCK-ST15
                    PERFORM  P-OPEN-WINDOW1
                    DISPLAY  STDLIC-3
                    PERFORM  P-CLEAR-F-TASTER
                    SET      NEJ TO TRUE
                    DISPLAY  STDLIC-3-INPUT
                    MOVE     4    TO EX-AFSNIT
                    MOVE     ZERO TO EX-FELTNR
                    ACCEPT   STDLIC-3-INPUT
                    PERFORM  P-SET-F-TASTER
                    PERFORM  P-CLOSE-WINDOW
                    IF       JA
                             GO       M3-LOGIN
                    ELSE
                             SET LS-STDLIC-INV-CALL TO TRUE.
      
       M3-CHK-ST14.
      * Denne funktion checker, om der forefindes en ny stdfil14.
      * I så tilfælde opdateres den med version fra profil01.
      * Ligeledes opdateres stdfil15 med antal licenser pr. modul.
           PERFORM  R-OPEN-ST14.
           PERFORM  R-OPEN-PD01-02.
      
           MOVE     ZERO  TO ST14-LICENS.
           MOVE     SPACE TO ST14-PRODUKT.
           PERFORM  START-NEXT-ST14.
      
           IF       VAL-ST14
                    PERFORM  READ-NEXT-ST14.
      
           IF       VAL-ST14
                AND ST14-PRODUKT NOT = SPACE
                AND ST14-VERSION     = SPACE
                    PERFORM  M3-NULSTIL-ST15
                    PERFORM  M3-OPDAT-ST14
                       UNTIL INV-ST14
      * Just in case!
                          OR ST14-PRODUKT = SPACE.
      
           PERFORM  UNLOCK-ST14.
           PERFORM  R-CLOSE-ST14.
           PERFORM  R-CLOSE-PD01-02.
      
       M3-NULSTIL-ST15.
           MOVE     SPACE TO ST15-MODUL.
           PERFORM  START-NEXT-ST15.
      
           PERFORM  M3-NULSTIL-ST15-RECORD UNTIL INV-ST15.
      
       M3-NULSTIL-ST15-RECORD.
           MOVE     SPACE  TO ST15-MODUL-LINK.
           MOVE     ZERO   TO ST15-ANTAL-BRUGERE
                              ST15-ANTAL-BRUGT
                              ST15-ANTAL-PROCESSER.
           MOVE     999999 TO ST15-DATO-SLUT.
           PERFORM  REWRITE-ST15.
      
           PERFORM  READ-NEXT-ST15.
      
       M3-OPDAT-ST14.
           MOVE     ST14-PRODUKT TO PD01-PRODUKT.
           MOVE     SPACE        TO PD01-NY-MENU.
           PERFORM  READ-PD01-NL.
      
           IF       VAL-PD01
                    MOVE     PD01-VERSION TO ST14-VERSION
                    ACCEPT   ST14-DATO-OPDAT FROM DATE
                    PERFORM  REWRITE-ST14.
      
           IF       VAL-ST14
                    MOVE     PD01-PRODUKT TO PD02-PRODUKT
                    MOVE     SPACE        TO PD02-MODUL
                    PERFORM  START-NEXT-PD02-NL
                    PERFORM  M3-OPDAT-ST15
                       UNTIL PD02-PRODUKT NOT = PD01-PRODUKT
                          OR INV-PD02
           ELSE
                    PERFORM  P-OPEN-WINDOW1
                    DISPLAY  STDLIC-7
                    PERFORM  P-CLEAR-F-TASTER
                    MOVE     SPACE TO WS-OK
                    DISPLAY  STDLIC-7-INPUT
                    MOVE     5    TO EX-AFSNIT
                    MOVE     ZERO TO EX-FELTNR
                    ACCEPT   STDLIC-7-INPUT
                    STOP     RUN.
      
           IF       VAL-ST14
                    PERFORM  READ-NEXT-ST14.
      
       M3-OPDAT-ST15.
           MOVE     PD02-MODUL TO ST15-MODUL.
           PERFORM  READ-ST15.
      
           IF       VAL-ST15
                    MOVE     PD02-MODUL-LINK TO ST15-MODUL-LINK
                    IF       ST15-MODUL-LINK = SPACE
                             MOVE     ST14-ANTAL-BRUGERE
                                           TO ST15-ANTAL-BRUGERE
                             MOVE     ZERO TO ST15-ANTAL-BRUGT
                                              ST15-ANTAL-PROCESSER
                             MOVE     ST14-DATO-SLUT
                                           TO ST15-DATO-SLUT
                    ELSE
                             MOVE     ZERO TO ST15-ANTAL-BRUGERE
                                              ST15-ANTAL-BRUGT
                                              ST15-ANTAL-PROCESSER
                             MOVE     ST14-DATO-SLUT
                                           TO ST15-DATO-SLUT
                    END-IF
                    PERFORM  REWRITE-ST15.
      
           PERFORM  READ-NEXT-PD02-NL.
      
       M3-TJEK-WEDO-DIR.
      *    Tjek om C:\wedo findes?
           MOVE     "C:\wedo" TO WS-DIR-NAVN.
           PERFORM  M3-TJEK-DIR.
      *    Tjek om C:\wedo\printarkiv findes?
           MOVE     "C:\wedo\printarkiv" TO WS-DIR-NAVN.
           PERFORM  M3-TJEK-DIR.
      
       M3-TJEK-DIR.
           MOVE     SPACE TO W-DESKTOP-DIR.
           STRING   "@[DISPLAY]:" DELIMITED BY SIZE
                    WS-DIR-NAVN   DELIMITED BY SIZE
                    INTO W-DESKTOP-DIR.
           CALL     "C$LIST-DIRECTORY"
                    USING LISTDIR-OPEN, W-DESKTOP-DIR, "*.*".
           MOVE     RETURN-CODE TO W-MYDIR.
           IF       W-MYDIR NOT = NULL
                    CALL     "C$LIST-DIRECTORY"
                    USING    LISTDIR-CLOSE, W-MYDIR
           ELSE 
                    PERFORM  DAN-DIR-DESKTOP.
      
       M3-HELP-EXIT.
           EXIT.
      
      ***********************
       M4-HELP SECTION.
      ***********************
       M4-STOP-RUN.
           MOVE     EX-MED  TO ST16-MED.
           MOVE     EX-DATO TO ST16-DATO.
           MOVE     EX-TID  TO ST16-TID.
           MOVE     SPACE   TO ST16-MODUL.
           PERFORM  START-NEXT-ST16.
      
           PERFORM  M3-SLET-MODULER UNTIL
                    ST16-TID  NOT = EX-TID
                 OR ST16-DATO NOT = EX-DATO
                 OR ST16-MED  NOT = EX-MED
                 OR INV-ST16.
      
           IF       EX-ADGANGSNIVEAU NOT = "9"
                    PERFORM  M4-LOGOUT.
      
           MOVE     EX-MED  TO ST16-MED.
           MOVE     EX-DATO TO ST16-DATO.
           MOVE     EX-TID  TO ST16-TID.
           MOVE     "STD"   TO ST16-MODUL.
           PERFORM  READ-ST16.
      
           IF       VAL-ST16
                    PERFORM  DELETE-ST16.
      
       M4-LOGOUT.
           MOVE     "STD" TO ST15-MODUL.
           PERFORM  READ-ST15.
      
           IF       ST15-ANTAL-PROCESSER > ZERO
                    SUBTRACT 1 FROM ST15-ANTAL-PROCESSER.
      
           MOVE     EX-MED TO ST16-MED.
           MOVE     ZERO   TO ST16-DATO
                              ST16-TID.
           MOVE     SPACE  TO ST16-MODUL.
           PERFORM  START-NEXT-ST16-NL.
      
           IF       VAL-ST16
                    PERFORM  READ-NEXT-ST16-NL.
      
           IF       INV-ST16
                 OR ST16-MED NOT = EX-MED
                    IF       ST15-ANTAL-BRUGT > ZERO
                             SUBTRACT 1 FROM ST15-ANTAL-BRUGT.
      
           PERFORM  REWRITE-ST15.
      
       M4-HELP-EXIT.
           EXIT.
      
      ***********************
       M5-HELP SECTION.
      ***********************
       M5-CHK-ME.
      * Checker om denne session stadig er aktiv.
      * Kaldes fra win000.
           MOVE     EX-MED  TO ST16-MED.
           MOVE     EX-DATO TO ST16-DATO.
           MOVE     EX-TID  TO ST16-TID.
           MOVE     "STD"   TO ST16-MODUL.
           PERFORM  READ-ST16-NL.
      
           IF       INV-ST16
                    PERFORM  P-OPEN-WINDOW1
                    DISPLAY  STDLIC-4
                    PERFORM  P-CLEAR-F-TASTER
                    MOVE     SPACE TO WS-OK
                    DISPLAY  STDLIC-4-INPUT
                    MOVE     5    TO EX-AFSNIT
                    MOVE     ZERO TO EX-FELTNR
                    ACCEPT   STDLIC-4-INPUT
                    STOP     RUN
           ELSE
                    SET      LS-STDLIC-VAL-CALL TO TRUE.
      
       M5-HELP-EXIT.
           EXIT.
      
      ***********************
       M6-HELP SECTION.
      ***********************
       M6-ADG-TYPE.
      * Benyttes i Finans.
      * M6-ADG-TYPE må kun kaldes, når der ER givet adgang til et
      * program. I en fremtidig version kunne der dannes en EXTERNAL
      * variabel, som blev sat af M1-INIT-CALL. På den måde kunne hele
      * denne funktion (M6-ADG-TYPE) udelades.
           MOVE     LS-STDLIC-MODUL TO ST17-MODUL.
           MOVE     LS-STDLIC-PRGM  TO ST17-PRGM.
           MOVE     EX-MED          TO ST17-MED.
           PERFORM  READ-ST17-NL.
      
           IF       INV-ST17
                    MOVE     SPACE TO ST17-MED
                    PERFORM  READ-ST17-NL
                    IF       INV-ST17
                             MOVE     SPACE  TO ST17-PRGM
                             MOVE     EX-MED TO ST17-MED
                             PERFORM  READ-ST17-NL
                             IF       INV-ST17
                                      MOVE     SPACE TO ST17-MED
                                      PERFORM  READ-ST17-NL.
      
           IF       VAL-ST17
                    MOVE     ST17-FUNK-TYPE TO LS-STDLIC-ADG-TYPE
           ELSE
                    MOVE     SPACE          TO LS-STDLIC-ADG-TYPE.
      
       M6-HELP-EXIT.
           EXIT.
      
      ***********************
       P-PICTURE SECTION.
      ***********************
       P-OPSTART.
      *    ACCEPT   WS-DEFAULT-FONT FROM STANDARD OBJECT "DEFAULT-FONT".
      
           MOVE     EX-MODUL TO W-EX-MODUL.
           CALL     "C$TOUPPER" USING W-EX-MODUL VALUE 3.
      
           MOVE     EX-AFSNIT TO W-GEM-AFSNIT.
           MOVE     EX-FELTNR TO W-GEM-FELTNR.
      
           MOVE     EX-PROGRAM TO WS-GEM-PROGRAM.
           MOVE     "STDLIC" TO EX-PROGRAM.
      
       P-OPEN-WINDOW1.
           DISPLAY  FLOATING WINDOW LINE 8 LINES 8 SIZE 40
                    CONTROL FONT WS-DEFAULT-FONT,
                    CELL SIZE ENTRY-FIELD FONT WS-DEFAULT-FONT,
                    COLOR 160
                    HANDLE WS-WIN-WINDOW.
           DISPLAY  FRAME "Licens-styring" POS 1,4
                    SIZE 39,4 CELLS LINES 8 CELLS COLOR 4103
                    RIMMED.
      
       P-OPEN-WINDOW2.
           DISPLAY  FLOATING WINDOW LINE 8 LINES 8 SIZE 40
                    CONTROL FONT WS-DEFAULT-FONT,
                    CELL SIZE ENTRY-FIELD FONT WS-DEFAULT-FONT,
                    COLOR 160
                    HANDLE WS-WIN-WINDOW.
           DISPLAY  FRAME "Adgangskontrol" POS 1,4
                    SIZE 39,4 CELLS LINES 8 CELLS COLOR 4103
                    RIMMED.
      
       P-OPEN-WINDOW3.
           DISPLAY  FLOATING WINDOW LINE 7 LINES 8 SIZE 40
                    CONTROL FONT WS-DEFAULT-FONT,
                    CELL SIZE ENTRY-FIELD FONT WS-DEFAULT-FONT,
                    COLOR 128
                    HANDLE WS-WIN-WINDOW.
           DISPLAY  FRAME "Licens-styring" POS 1,4
                    SIZE 39,4 CELLS LINES 8 CELLS COLOR 4103
                    RIMMED.
      
       P-CLOSE-WINDOW.
           CLOSE    WINDOW WS-WIN-WINDOW.
      
       P-AFSLUT.
           MOVE     W-GEM-AFSNIT   TO EX-AFSNIT.
           MOVE     W-GEM-FELTNR   TO EX-FELTNR.
           MOVE     WS-GEM-PROGRAM TO EX-PROGRAM.
      
       P-CLEAR-F-TASTER.
           SET      ENVIRONMENT "KEYSTROKE" TO "Invalid=Yes k3".
           SET      ENVIRONMENT "KEYSTROKE" TO "Invalid=Yes k4".
           SET      ENVIRONMENT "KEYSTROKE" TO "Invalid=Yes k5".
           SET      ENVIRONMENT "KEYSTROKE" TO "Invalid=Yes k6".
           SET      ENVIRONMENT "KEYSTROKE" TO "Invalid=Yes k7".
           SET      ENVIRONMENT "KEYSTROKE" TO "Invalid=Yes k8".
           SET      ENVIRONMENT "KEYSTROKE" TO "Invalid=Yes k9".
           SET      ENVIRONMENT "KEYSTROKE" TO "Invalid=Yes k0".
      
       P-SET-F-TASTER.
           SET      ENVIRONMENT "KEYSTROKE" TO "Exception=3 k3".
           SET      ENVIRONMENT "KEYSTROKE" TO "Exception=4 k4".
           SET      ENVIRONMENT "KEYSTROKE" TO "Exception=5 k5".
           SET      ENVIRONMENT "KEYSTROKE" TO "Exception=6 k6".
           SET      ENVIRONMENT "KEYSTROKE" TO "Exception=7 k7".
           SET      ENVIRONMENT "KEYSTROKE" TO "Exception=8 k8".
           SET      ENVIRONMENT "KEYSTROKE" TO "Exception=19 k9".
           SET      ENVIRONMENT "KEYSTROKE" TO "Exception=10 k0".
      
       P-EXIT.
           EXIT.
      
      ***********************
       R-FILE SECTION.
      ***********************
           COPY     "pdpasswd.cpy".
           COPY     "pdprof01.cpy".
           COPY     "pdprof02.cpy".
           COPY     "pdstdf12.cpy".
           COPY     "pdstdf14.cpy".
           COPY     "pdstdf15.cpy".
           COPY     "pdstdf16.cpy".
           COPY     "pdstdf17.cpy".
           COPY     "pdstdf19.cpy".
           COPY     "pdstdf96.cpy".
           COPY     "pdstdf97.cpy".
       OPEN-IO-ST15.
           OPEN     I-O STDF15.
      
       UNLOCK-ST15.
           UNLOCK   STDF15.
      
       START-ST15.
           SET      VAL-ST15 TO TRUE.
      
           START    STDF15 KEY NOT LESS ST15-KEY
                    INVALID KEY
                    SET INV-ST15 TO TRUE.
      
           IF       BUSSY
                    GO START-ST15.
      
       START-NEXT-ST15.
           PERFORM  START-ST15.
      
           IF       VAL-ST15
                    PERFORM  READ-NEXT-ST15.
      
       READ-NEXT-ST15.
           SET      VAL-ST15 TO TRUE.
      
           READ     STDF15 NEXT
                    AT END
                    SET INV-ST15 TO TRUE.
      
           IF       BUSSY
                    GO READ-NEXT-ST15.
      
       READ-ST15.
           SET      VAL-ST15 TO TRUE.
      
           READ     STDF15
                    INVALID KEY
                    SET INV-ST15 TO TRUE.
      
           IF       BUSSY
                    GO READ-ST15.
      
       REWRITE-ST15.
           SET      VAL-ST15 TO TRUE.
      
           REWRITE  ST15-REC
                    INVALID KEY
                    SET INV-ST15 TO TRUE.
      
       R-OPEN-ST12.
           PERFORM  OPEN-I-ST12.
      
       R-CLOSE-ST12.
           PERFORM  CLOSE-ST12.
      
       R-OPEN-ST14.
           PERFORM  OPEN-IO-ST14.
      
       R-CLOSE-ST14.
           PERFORM  CLOSE-ST14.
      
       R-OPEN-PD01-02.
           PERFORM  OPEN-I-PD01.
           PERFORM  OPEN-I-PD02.
      
       R-CLOSE-PD01-02.
           PERFORM  CLOSE-PD01.
           PERFORM  CLOSE-PD02.
      
       R-OPEN.
           PERFORM  OPEN-IO-ST15.
           PERFORM  OPEN-IO-ST16.
           PERFORM  OPEN-I-ST17.
           PERFORM  OPEN-IO-ST19.
           PERFORM  OPEN-ST96.
           PERFORM  OPEN-ST97.
      
       R-CLOSE.
           PERFORM  CLOSE-ST15.
           PERFORM  CLOSE-ST16.
           PERFORM  CLOSE-ST17.
           PERFORM  CLOSE-ST19.
           PERFORM  CLOSE-ST96.
           PERFORM  CLOSE-ST97.
      
       R-EXIT.
           EXIT.
      
      ***********************
       STD SECTION.
      ***********************
           COPY     "pdchkkey.cpy".
           COPY     "pddato.cpy".
           COPY     "pdsystem.cpy".
      *    COPY     "userror.cpy".
       USE-ERROR.
           IF       BUSSY
                 OR FILE-LOCKED
                    PERFORM  USE-BUSSY-G
           ELSE
                    PERFORM  USE-ERROR-G
      *             CALL     "stdxit"
                    STOP RUN.
      
       USE-BUSSY-G.
           DISPLAY  FLOATING WINDOW SIZE 40 LINES 4
                    TITLE "WeDo"
                    CELL SIZE ENTRY-FIELD FONT WS-DEFAULT-FONT,
                    BLANK COLOR 160
                    HANDLE WS-ERROR-WINDOW.
           DISPLAY  FRAME "System Besked" POS 1,4
                    SIZE 39,4 CELLS LINES 4 CELLS COLOR 4103
                    RIMMED.
           DISPLAY  LABEL "OPTAGET:" AT 0303 COLOR 4104.
           DISPLAY  LABEL US-FILNAVN AT 0312 COLOR 4104.
           ACCEPT   OMITTED AT 0440 TIME 200.
           CLOSE    WINDOW WS-ERROR-WINDOW.
      
       USE-ERROR-G.
           DISPLAY  FLOATING WINDOW SIZE 44 LINES 11
                    TITLE "WeDo"
                    CELL SIZE ENTRY-FIELD FONT WS-DEFAULT-FONT,
                    BLANK COLOR 160
                    HANDLE WS-ERROR-WINDOW.
           DISPLAY  FRAME "System Besked" POS 1,4
                    SIZE 43,4 CELLS LINES 11 CELLS COLOR 4103
                    RIMMED.
           DISPLAY  LABEL "Der er opstået en fejl!"
                                     AT 0303 COLOR 4104.
           DISPLAY  LABEL "Program." AT 0403 COLOR 4103.
           DISPLAY  LABEL EX-TITEL   AT 0418 COLOR 4104.
           DISPLAY  LABEL "ProgramNr."
                                     AT 0503 COLOR 4103.
           DISPLAY  LABEL EX-PROGRAM AT 0518 COLOR 4104.
           DISPLAY  LABEL "Filnavn." AT 0603 COLOR 4103.
           DISPLAY  LABEL US-FILNAVN AT 0618 COLOR 4104.
           DISPLAY  LABEL "Fejl."    AT 0703 COLOR 4103.
      
           IF       US-STAT = "94"
                    MOVE     "Too many files open." TO US-TXT
           ELSE
                    CALL     "stdfsc" USING US-STAT US-TXT
                    CANCEL   "stdfsc".
      
           DISPLAY  LABEL US-TXT    AT 0718 COLOR 4104.
           DISPLAY  LABEL "FejlNr." AT 0803 COLOR 4103.
           DISPLAY  LABEL US-STAT   AT 0818 COLOR 4104.
           DISPLAY  LABEL "Tast <Return>:"
                                    AT 1003 COLOR 4104.
           DISPLAY  OMITTED AT 1018 BEEP.
           ACCEPT   OMITTED.
           CLOSE    WINDOW WS-ERROR-WINDOW.
      
       STD-EXIT.
           EXIT.
