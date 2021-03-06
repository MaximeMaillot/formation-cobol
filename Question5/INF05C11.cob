       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB5.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-MVT ASSIGN
            DDMVT FILE STATUS IS CR-MVT.
      ****************************************************************
      *    D A T A   D I V I S I O N
      ***********************************************************
       DATA DIVISION.
       FILE SECTION.
       fd F-MVT
           BLOCK CONTAINS 0
           record contains 80
           recording mode F
           DATA RECORD IS E-MVT.
       01 E-MVT.
         05 MATRICULE        PIC 9(6).
         05 CODE-MOUVEMENT   PIC 9(1).
         05 CODE-N           PIC X(73).
         05 CODE-1 REDEFINES CODE-N.
           10 SIGNE            PIC X(1).
           10 TAUX             PIC 9(2).
           10 PIC X(70).
         05 CODE-2 REDEFINES CODE-N.
           10 NOM-PRENOM       PIC X(20).
           10 PIC X(53).
         05 CODE-3 REDEFINES CODE-N.
           10 ADRESSE          PIC X(18).
           10 CODE-POSTAL      PIC 9(5).
           10 VILLE            PIC X(12).
           10 PIC X(38).
         05 CODE-4 REDEFINES CODE-N.
           10 TYPE-P           PIC X(1).
           10 PRIME            PIC 9(4)V9(2).
           10 PIC X(66).
           
      ****************************************************************
      *    W O R K I N G - S T O R A G E   S E C T I O N
      ****************************************************************
       WORKING-STORAGE SECTION.
      *--------------DEFINITION DES VARIABLES---------------------
       01 CR-MVT PIC 99.
      * ---------- FORMAT ----------- 
       01 FORMAT-DATE.
         05  DATE-N.
           10 YEAR-N         PIC 9(4).
           10 MONTH-N        PIC 9(2).
           10 DAY-N          PIC 9(2).
         05 TIME-N.
           10 HOUR-N         PIC 9(2).
           10 MINUTE-N       PIC 9(2).
           10 SECOND-N       PIC 9(2).
         05 WEEKDAY-N        PIC 9.
       01 WEEKDAY-DESC.
         05 PIC X(10) VALUE 'LUNDI'.
         05 PIC X(10) VALUE 'MARDI'.
         05 PIC X(10) VALUE 'MERCREDI'.
         05 PIC X(10) VALUE 'JEUDI'.
         05 PIC X(10) VALUE 'VENDREDI'.
         05 PIC X(10) VALUE 'SAMEDI'.
         05 PIC X(10) VALUE 'DIMANCHE'.
       01 WEEKDAY-TABLE redefines WEEKDAY-DESC.
         05 W-NAME PIC X(10) occurs 7.
       01 HEADER-F.
         05 APIN             PIC X(38).
         05 WEEKDAY-F        PIC X(10).
         05 SEPARATOR-F      PIC X(4).
         05 DATE-F           PIC X(20).
       01 TITLE-F.
         05                  PIC X(20).
         05 MAIN-TITLE       PIC X(32).
         05                  PIC X(20).
       01 ERROR-F.
         05 MATRICULE-M      PIC X(11).
         05                  PIC X(2).
         05 TYPE-M           PIC X(7).
         05                  PIC X(2).
         05 ERROR-M          PIC X(50).
       01 ERROR-CPT-F.
         05 DESCRIPTION      PIC X(50).
         05                  PIC X(3) VALUE ' : '.
         05 CPT-ERROR-N      PIC 9(4).
      * -------- COMPTEURS ----------   
       01 CPT.
         05 CPT-MVT          PIC 9(4) VALUE 0.
         05 CPT-ERROR-TOT    PIC 9(4) VALUE 0.
         05 CPT-ERROR-1      PIC 9(4) VALUE 0.
         05 CPT-ERROR-2      PIC 9(4) VALUE 0.
         05 CPT-ERROR-3      PIC 9(4) VALUE 0.
         05 CPT-ERROR-4      PIC 9(4) VALUE 0.
         05 CPT-ERROR-MAT    PIC 9(4) VALUE 0.
         05 CPT-ERROR-CODE   PIC 9(4) VALUE 0.
       77 HAS-ERROR            PIC 9 VALUE 0.
       77 EOF-TRUE             PIC X VALUE "Y".
       77 EOF                  PIC X VALUE "F".


      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           PERFORM CONSTRUCT-HEADER
           OPEN INPUT F-MVT
           PERFORM UNTIL EOF = EOF-TRUE
             READ F-MVT
                AT END
                    MOVE EOF-TRUE TO EOF
                NOT AT END
                   PERFORM MOUVEMENT-L
             END-READ
           END-PERFORM
           PERFORM DISPLAY-STATS
           CLOSE F-MVT
           STOP RUN.
      * PARCOURS MOUVEMENT
       MOUVEMENT-L.
           IF MATRICULE IS NOT NUMERIC
              MOVE "1 - Matricule non numerique" TO ERROR-M
              ADD 1 TO CPT-ERROR-MAT
              PERFORM HAS-ERROR-P
           ELSE
      * Switch case
            EVALUATE CODE-MOUVEMENT
             WHEN 1
               IF SIGNE NOT EQUAL '-' AND SIGNE NOT EQUAL '+'
                 MOVE "3 - Signe different de + ou -" TO ERROR-M
                 ADD 1 TO CPT-ERROR-1
                 PERFORM HAS-ERROR-P
               ELSE
                IF TAUX IS NOT NUMERIC
                  ADD 1 TO CPT-ERROR-1
                  MOVE "4 - Taux non numerique" TO ERROR-M
                  PERFORM HAS-ERROR-P
                END-IF
               END-IF
             WHEN 2
               IF NOM-PRENOM EQUAL SPACE
                 MOVE "5 - Le nom n'est pas renseigne" TO ERROR-M
                 ADD 1 TO CPT-ERROR-2
                 PERFORM HAS-ERROR-P
               END-IF
             WHEN 3
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
             WHEN 4
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
             WHEN OTHER
               MOVE "2 - Code mouvement inconnu" TO ERROR-M
               ADD 1 TO CPT-ERROR-CODE
               PERFORM HAS-ERROR-P
            END-EVALUATE
           END-IF
           IF HAS-ERROR EQUAL 1
               DISPLAY ERROR-F
           END-IF
           MOVE 0 TO HAS-ERROR
           ADD 1 TO CPT-MVT
           .
       HAS-ERROR-P.
           ADD 1 TO CPT-ERROR-TOT
           MOVE MATRICULE TO MATRICULE-M
           MOVE CODE-MOUVEMENT TO TYPE-M
           MOVE 1 TO HAS-ERROR
           .
       CONSTRUCT-HEADER.
           PERFORM GET-DATE
           MOVE "API11" TO APIN
           MOVE W-NAME (WEEKDAY-N) TO WEEKDAY-F
           MOVE " le " TO SEPARATOR-F
           STRING
            DAY-N DELIMITED BY SIZE
            "/" DELIMITED BY SIZE
            MONTH-N DELIMITED BY SIZE
            "/" DELIMITED BY SIZE
            YEAR-N DELIMITED BY SIZE
            INTO DATE-F
           END-STRING
           DISPLAY HEADER-F
           DISPLAY SPACE
           INITIALIZE HEADER-F
           STRING
            HOUR-N DELIMITED BY SIZE
            ":" DELIMITED BY SIZE
            MINUTE-N DELIMITED BY SIZE
            "." DELIMITED BY SIZE
            SECOND-N DELIMITED BY SIZE
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
           ACCEPT DATE-N FROM DATE YYYYMMDD
           ACCEPT TIME-N FROM TIME
           ACCEPT WEEKDAY-N FROM DAY-OF-WEEK
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
           MOVE "    16 - Nombre d erreur mat invalide" TO DESCRIPTION
           MOVE CPT-ERROR-MAT TO CPT-ERROR-N
           DISPLAY ERROR-CPT-F
           MOVE "    17 - Nombre d erreur code invalide" TO DESCRIPTION
           MOVE CPT-ERROR-CODE TO CPT-ERROR-N
           DISPLAY ERROR-CPT-F
           .
