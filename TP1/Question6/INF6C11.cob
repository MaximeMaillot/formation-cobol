       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB5.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-MVTMAJ ASSIGN
            DDMVTMAJ FILE STATUS IS CR-MVTMAJ.
      ****************************************************************
      *    D A T A   D I V I S I O N
      ***********************************************************
       DATA DIVISION.
       FILE SECTION.
       fd F-MVTMAJ
           BLOCK CONTAINS 0
           DATA RECORD IS E-MVTMAJ.
       01 E-MVTMAJ                PIC X(80).
      ****************************************************************
      *    W O R K I N G - S T O R A G E   S E C T I O N
      ****************************************************************
       WORKING-STORAGE SECTION.
       01 CR-MVTMAJ PIC 99.
      *-------------- MOUVEMENT  ---------------------
       01 W-MOUVEMENT.
           05 MATRICULE        PIC 9(6).
           05 CODE-MOUVEMENT   PIC 9(1).
           05 CODE-N           PIC X(73).
       01 CODE-1.
           05 SIGNE            PIC X(1).
           05 TAUX             PIC 9(2).
       01 CODE-2.
           05 NOM-PRENOM       PIC X(20).
       01 CODE-3.
           05 ADRESSE          PIC X(18).
           05 CODE-POSTAL      PIC 9(5).
           05 VILLE            PIC X(12).
       01 CODE-4.
           05 TYPE-P           PIC X(1).
           05 PRIME            PIC 9(4)V9(2).
       01 CODE-5.
           05 NOM              PIC A(12).
           05 PRENOM           PIC A(10).
           05 TEL-FIXE         PIC 9(10).
           05 TEL-PORT         PIC 9(10).
           05 SALAIRE          PIC S9(4)V9(2) COMP-3.
           05 QUALIFICATION    PIC X(19).
           05 DATE-EMBAUCHE    PIC 9(8).
      * ------------------------------------------------------
      * Diviser une date DDMMYYYY 
       01 DATE-CONVERT.
           05 DAY-CONV         PIC 99.
           05 MONTH-CONV       PIC 99.
           05 YEAR-CONV        PIC 9999.
      * Diviser un numero de téléphone
       01 TEL-CONVERT.
           05 TEL-CONV         PIC 99.
           05                  PIC 9(8).
      * Variable d'edition d'un salaire     
       01 SALAIRE-EDIT         PIC z(6)9,99.
      * Date actuelle 
       01 CURR-DATE.
           05 DATE-C.
             10 YEAR-C         PIC 9(4).
             10 MONTH-C        PIC 9(2).
             10 DAY-C          PIC 9(2).
           05 TIME-C.
             10 HOUR-C         PIC 9(2).
             10 MINUTE-C       PIC 9(2).
             10 SECOND-C       PIC 9(2).
           05 WEEKDAY-C        PIC 9.
      * Compteurs     
       01 CPT.
           05 CPT-MVT          PIC 9(4) VALUE 0.
           05 CPT-ERROR-TOT    PIC 9(4) VALUE 0.
           05 CPT-ERROR-1      PIC 9(4) VALUE 0.
           05 CPT-ERROR-2      PIC 9(4) VALUE 0.
           05 CPT-ERROR-3      PIC 9(4) VALUE 0.
           05 CPT-ERROR-4      PIC 9(4) VALUE 0.
           05 CPT-ERROR-5      PIC 9(4) VALUE 0.
           05 CPT-ERROR-MAT    PIC 9(4) VALUE 0.
           05 CPT-ERROR-CODE   PIC 9(4) VALUE 0.
      * -------------------- HEADER -------------- 
       01 HEADER-F.
           05 APIN             PIC X(38).
           05 WEEKDAY-F        PIC X(10).
           05 SEPARATOR-F      PIC X(4).
           05 DATE-F           PIC X(20).   
       01 TITLE-F.
           05                  PIC X(20).
           05 MAIN-TITLE       PIC X(32).
           05                  PIC X(20).
      * --------------------------------------------               
      * Affichage d'une erreur
       01 ERROR-F.
           05 MATRICULE-M      PIC X(11).
           05                  PIC X(2).
           05 TYPE-M           PIC X(7).
           05                  PIC X(2).
           05 ERROR-M          PIC X(50).
      * Affichage des erreurs dans les compteurs       
       01 ERROR-CPT-F.
           05 DESCRIPTION      PIC X(50).
           05                  PIC X(3) VALUE ' : '.
           05 CPT-ERROR-N      PIC 9(4).
       77 WEEKDAY-NAME          PIC X(10).
      * Variable de parcours d'un fichier 
       77 EOF-TRUE             PIC X VALUE "Y".
       77 EOF                  PIC X VALUE "F".


      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           OPEN INPUT F-MVTMAJ
           PERFORM CONSTRUCT-HEADER
           PERFORM UNTIL EOF = EOF-TRUE
             READ F-MVTMAJ INTO W-MOUVEMENT
              AT END
                MOVE EOF-TRUE TO EOF
              NOT AT END
                PERFORM MOUVEMENT-L
             END-READ
           END-PERFORM
           PERFORM DISPLAY-STATS
           STOP RUN.
      * PARCOURS MOUVEMENT
       MOUVEMENT-L.
           ADD 1 TO CPT-MVT
           IF MATRICULE IS NOT NUMERIC
             MOVE "1 - Matricule non numerique" TO ERROR-M
             ADD 1 TO CPT-ERROR-MAT  
             PERFORM HAS-ERROR-P
           ELSE
             PERFORM EVALUATE-MVT
           END-IF
           .
      * Check the MVT Code
       EVALUATE-MVT.
           EVALUATE CODE-MOUVEMENT
             WHEN 1
               MOVE CODE-N TO CODE-1
               PERFORM MVT-CODE-1
             WHEN 2
               MOVE CODE-N TO CODE-2
               PERFORM MVT-CODE-2
             WHEN 3
               MOVE CODE-N TO CODE-3
               PERFORM MVT-CODE-3
             WHEN 4
               MOVE CODE-N TO CODE-4
               PERFORM MVT-CODE-4
             WHEN 5
               MOVE CODE-N TO CODE-5
               PERFORM MVT-CODE-5
             WHEN OTHER
               MOVE "2 - Code mouvement inconnu" TO ERROR-M
               PERFORM HAS-ERROR-P
               ADD 1 TO CPT-ERROR-CODE
            END-EVALUATE
           .
       MVT-CODE-1.
           IF SIGNE NOT EQUAL '-' AND SIGNE NOT EQUAL '+'
             MOVE "3 - Signe different de + ou -" TO ERROR-M
             PERFORM HAS-ERROR-P
             ADD 1 TO CPT-ERROR-1
           ELSE
             IF TAUX IS NOT NUMERIC
               MOVE "4 - Taux non numerique" TO ERROR-M
               PERFORM HAS-ERROR-P
               ADD 1 TO CPT-ERROR-1
             END-IF
           END-IF
           .
       MVT-CODE-2.
           IF NOM-PRENOM EQUAL SPACE
             MOVE "5 - Le nom n'est pas renseigne" TO ERROR-M
             PERFORM HAS-ERROR-P
             ADD 1 TO CPT-ERROR-2
           END-IF
           .
       MVT-CODE-3.
           IF ADRESSE EQUAL SPACE
            AND CODE-POSTAL EQUAL SPACE
            AND VILLE EQUAL SPACE
             MOVE "6 - Au moins une zone doit etre saisie"
              TO ERROR-M
             ADD 1 TO CPT-ERROR-3
             PERFORM HAS-ERROR-P
             ELSE
               IF CODE-POSTAL IS NOT NUMERIC
                AND CODE-POSTAL NOT EQUAL SPACE
                 MOVE "7 - Code postal invalide" TO ERROR-M
                 ADD 1 TO CPT-ERROR-3
                 PERFORM HAS-ERROR-P
               END-IF
           END-IF
           .
       MVT-CODE-4.
           IF TYPE-P IS NOT NUMERIC
             MOVE "8 - Type de prime invalide" TO ERROR-M
             ADD 1 TO CPT-ERROR-4
             PERFORM HAS-ERROR-P
           ELSE
             IF PRIME IS NOT NUMERIC
               MOVE "9 - Prime non numerique" TO ERROR-M
               ADD 1 TO CPT-ERROR-4
               PERFORM HAS-ERROR-P
             END-IF
           END-IF
           .
       MVT-CODE-5.
      *    Test telephone fixe 
           MOVE TEL-FIXE TO TEL-CONVERT
           IF (TEL-CONV < 1) OR (TEL-CONV > 7)
             MOVE "10 - Telephone fixe incorrect" TO ERROR-M
             ADD 1 TO CPT-ERROR-5
             PERFORM HAS-ERROR-P
           ELSE
      *      Test telephone portable       
             MOVE TEL-PORT TO TEL-CONVERT
             IF (TEL-CONV < 6) OR (TEL-CONV > 7)
               MOVE "11 - Telephone portable incorrect" TO ERROR-M
               ADD 1 TO CPT-ERROR-5
               PERFORM HAS-ERROR-P
             ELSE
      *        Test date d'embauche    
               MOVE DATE-EMBAUCHE TO DATE-CONVERT
               IF DAY-CONV < 0 OR DAY-CONV > 31 OR
                 MONTH-CONV < 0 OR MONTH-CONV > 12
                 MOVE "12 - Date d'embauche incorrect" TO ERROR-M
                 ADD 1 TO CPT-ERROR-5
                 PERFORM HAS-ERROR-P             
               ELSE
      *        Affiche le salaire 
                 MOVE SALAIRE TO SALAIRE-EDIT
                 DISPLAY MATRICULE ":" NOM ":" SALAIRE-EDIT                 
               END-IF
             END-IF
           END-IF
           .
      * Populate an error format
       HAS-ERROR-P.
           MOVE MATRICULE TO MATRICULE-M
           MOVE CODE-MOUVEMENT TO TYPE-M
           DISPLAY ERROR-F
           ADD 1 TO CPT-ERROR-TOT
           .
       CONSTRUCT-HEADER.
           PERFORM GET-DATE
           MOVE "API11" TO APIN
           MOVE WEEKDAY-NAME TO WEEKDAY-F
           MOVE " le " TO SEPARATOR-F 
           STRING
            DAY-C DELIMITED BY SIZE
            "/" DELIMITED BY SIZE
            MONTH-C DELIMITED BY SIZE
            "/" DELIMITED BY SIZE
            YEAR-C DELIMITED BY SIZE
            INTO DATE-F
           END-STRING
           DISPLAY HEADER-F
           DISPLAY SPACE
           INITIALIZE APIN
           INITIALIZE SEPARATOR-F
           INITIALIZE WEEKDAY-F
           INITIALIZE DATE-F
           STRING 
            HOUR-C DELIMITED BY SIZE
            ":" DELIMITED BY SIZE
            MINUTE-C DELIMITED BY SIZE
            "." DELIMITED BY SIZE
            SECOND-C DELIMITED BY SIZE
            INTO DATE-F
           END-STRING
           DISPLAY HEADER-F
           DISPLAY SPACE
           MOVE "LISTE DES ERREURS" TO MAIN-TITLE
           DISPLAY TITLE-F
           MOVE "--------------------------------" TO MAIN-TITLE
           DISPLAY TITLE-F
           DISPLAY SPACE
           MOVE "MATRICULE" TO MATRICULE-M
           MOVE "TYPE" TO TYPE-M
           MOVE "MESSAGE D'ERREUR" TO ERROR-M
           DISPLAY ERROR-F
           MOVE "----------------------" TO MATRICULE-M
           MOVE "----------------------" TO TYPE-M
           MOVE "----------------------" TO ERROR-M
           DISPLAY ERROR-F
           .
       GET-DATE.
           ACCEPT DATE-C FROM DATE YYYYMMDD
           ACCEPT TIME-C FROM TIME
           ACCEPT WEEKDAY-C FROM DAY-OF-WEEK
           EVALUATE WEEKDAY-C
             WHEN 1
               MOVE "LUNDI" TO WEEKDAY-NAME
             WHEN 2
               MOVE "MARDI" TO WEEKDAY-NAME
             WHEN 3
               MOVE "MERCREDI" TO WEEKDAY-NAME
             WHEN 4
               MOVE "JEUDI" TO WEEKDAY-NAME
             WHEN 5
               MOVE "VENDREDI" TO WEEKDAY-NAME
             WHEN 6
               MOVE "SAMEDI" TO WEEKDAY-NAME
             WHEN 7
               MOVE "DIMANCHE" TO WEEKDAY-NAME
           .
       DISPLAY-STATS.
           DISPLAY SPACE
           MOVE "Statistique sur controle du fichier mouvement"
            TO MAIN-TITLE
           DISPLAY TITLE-F
           MOVE "--------------------------------" TO MAIN-TITLE
           DISPLAY TITLE-F
           DISPLAY SPACE
           MOVE "10 - Nombre d enregistrements lu sur FMVT" 
            TO DESCRIPTION
           MOVE CPT-MVT TO CPT-ERROR-N
           DISPLAY ERROR-CPT-F
           MOVE "11 - Nombre d enregistrements en erreur" TO DESCRIPTION
           MOVE CPT-ERROR-TOT TO CPT-ERROR-N
           DISPLAY ERROR-CPT-F
           MOVE "    12 - Nombre d erreur de type 1" TO DESCRIPTION
           MOVE CPT-ERROR-1 TO CPT-ERROR-N
           DISPLAY ERROR-CPT-F
           MOVE "    13 - Nombre d erreur de type 2" TO DESCRIPTION
           MOVE CPT-ERROR-2 TO CPT-ERROR-N
           DISPLAY ERROR-CPT-F
           MOVE "    14 - Nombre d erreur de type 3" TO DESCRIPTION
           MOVE CPT-ERROR-3 TO CPT-ERROR-N
           DISPLAY ERROR-CPT-F
           MOVE "    15 - Nombre d erreur de type 4" TO DESCRIPTION
           MOVE CPT-ERROR-4 TO CPT-ERROR-N
           DISPLAY ERROR-CPT-F
           MOVE "    16 - Nombre d erreur de type 5" TO DESCRIPTION
           MOVE CPT-ERROR-5 TO CPT-ERROR-N
           DISPLAY ERROR-CPT-F
           MOVE "    17 - Nombre d erreur mat invalide" TO DESCRIPTION
           MOVE CPT-ERROR-MAT TO CPT-ERROR-N
           DISPLAY ERROR-CPT-F
           MOVE "    18 - Nombre d erreur code invalide" TO DESCRIPTION
           MOVE CPT-ERROR-CODE TO CPT-ERROR-N
           DISPLAY ERROR-CPT-F
           .
