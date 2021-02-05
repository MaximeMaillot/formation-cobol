       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB7.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-ASSURES ASSIGN
            DDASSUR FILE STATUS IS CR-ASSURES.
      *********************************
      *    D A T A   D I V I S I O N
      *********************************
       DATA DIVISION.

       FILE SECTION.
       fd F-ASSURES
           BLOCK CONTAINS 0
           record contains 80
           recording mode f
           DATA RECORD IS E-ASSURES.
       01 E-ASSURES.
         05  MATRICULE             PIC 9(6).
         05  NOM-PRENOM            PIC X(20).
         05  ADRESSE.
           10  RUE-ADRESSE         PIC X(18).
           10  CODE-POSTAL.
             15 DEPARTEMENT        PIC 9(2).
             15                    PIC 9(3).
           10  VILLE               PIC X(12).
         05  ASSURANCE.
           10  TYPE-VEHICULE       PIC X(1).
           10  PRIME-DE-BASE       PIC 9(4)V9(2).
           10  BONUS-MALUS         PIC X(1).
           10  TAUX                PIC 9(2).
         05 PIC X(9).

       WORKING-STORAGE SECTION.
       01 CR-ASSURES               PIC 99.
       77 EOF-TRUE                 PIC X VALUE "Y".
       77 EOF                      PIC X VALUE "F".

      * ------------ Totaux --------- 
       77 TOT-TYPE                 PIC 9(5)V9(2) VALUE 0.     
       77 TOT-DEPT                 PIC 9(5)V9(2) VALUE 0.
       77 TOT-GEN                  PIC 9(5)V9(2) VALUE 0.

      * ----------- Variable temporaire --------- 
       77 TYPE-TEMP                PIC X(1) VALUE '1'.
       77 DEPT-TEMP                PIC 9(2) VALUE 00.

      * --------------- FORMAT --------------
       01 HEADER-FORMAT.
           05                      PIC X(15).
           05                      PIC X(42) 
            VALUE 'Statistique par departement'.
           05                      PIC X(15).
       01 MONTANT-HEADER-FORMAT.
           05                      PIC X(10).
           05                      PIC X(20) VALUE 'NOM / PRENOM'.
           05                      PIC X(30).
           05                      PIC X(12) VALUE 'MONTANT'.
       01 DEPT-FORMAT.
           05                      PIC X(15) VALUE 'DEPARTEMENT'.
           05                      PIC X(1) VALUE ':'.
           05                      PIC X(5).
           05 DEPT-F               PIC 9(2).
       01 TYPE-FORMAT.
           05                      PIC X(15) VALUE 'TYPE'.
           05                      PIC X(1) VALUE ':'.
           05                      PIC X(5).
           05 TYPE-F               PIC X(1).
       01 MONTANT-FORMAT.
           05                      PIC X(10).
           05 NOM-PRENOM-F         PIC X(20).
           05                      PIC X(30).
           05 MONTANT-F            PIC Z(5)9,99.
       01 TOT-FORMAT.
           05 TOT-N                PIC X(20).
           05                      PIC X(1) VALUE ':'.
           05                      PIC X(39).
           05 TOT-F                PIC Z(5)9,99.
       77 LIGNE                    PIC X(80) VALUE ALL '-'.
       01 HALF-LIGNE.
           05                      PIC X(10).
           05                      PIC X(10) VALUE '----------'.    

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           PERFORM 10000-INIT
           PERFORM 20000-TRAITEMENT
           PERFORM 30000-FIN
           STOP RUN.
      * Ouvre le fichier
       10000-INIT.
           OPEN INPUT F-ASSURES
           PERFORM 11000-CONSTRUCT-HEADER
           .
      * Parcours le fichier
       20000-TRAITEMENT.
           PERFORM UNTIL EOF = EOF-TRUE
             READ F-ASSURES
               AT END
                 MOVE EOF-TRUE TO EOF
               NOT AT END
                 PERFORM 21000-ASSURES-L 
             END-READ
           END-PERFORM
           .
      * Ferme le fichier et affiche les totaux
       30000-FIN.
           CLOSE F-ASSURES
           PERFORM 21210-DISPLAY-TOT-TYPE
           ADD TOT-TYPE TO TOT-DEPT
           ADD TOT-DEPT TO TOT-GEN
           PERFORM 21110-DISPLAY-TOT-DEPT
           PERFORM 31000-DISPLAY-TOT-GEN
           .
      * Affiche des informations en haut de la page
       11000-CONSTRUCT-HEADER.
           DISPLAY HEADER-FORMAT
           DISPLAY SPACE
           DISPLAY LIGNE
           DISPLAY SPACE
           .
      * Boucle principale
       21000-ASSURES-L.
      *    Si on ne change pas de departement ni de type de vehicule
           IF DEPT-TEMP = DEPARTEMENT AND TYPE-TEMP = TYPE-VEHICULE
             PERFORM 21500-DISPLAY-MONTANT
             PERFORM 21600-ADD-MONTANT
           END-IF
      *    Si on ne change pas de departement 
      *    mais on change de type de vehicule     
           IF DEPT-TEMP = DEPARTEMENT AND TYPE-TEMP NOT = TYPE-VEHICULE
             PERFORM 21200-CHANGE-TYPE
           END-IF
      *    Si c'est la 1ère boucle
           IF TYPE-TEMP = '1' AND DEPT-TEMP = 00
             PERFORM 21300-DISPLAY-DEPT
             PERFORM 21400-DISPLAY-TYPE
             DISPLAY MONTANT-HEADER-FORMAT
             PERFORM 21500-DISPLAY-MONTANT
             PERFORM 21600-ADD-MONTANT
             MOVE DEPARTEMENT TO DEPT-TEMP
             MOVE TYPE-VEHICULE TO TYPE-TEMP
           END-IF
      *    Si on change de departement     
           IF DEPT-TEMP NOT = DEPARTEMENT
             PERFORM 21100-CHANGE-DEPT
           END-IF  
           .
      * Affiche le departement
       21300-DISPLAY-DEPT.
           MOVE DEPT-TEMP TO DEPT-F
           DISPLAY DEPT-FORMAT
           .
      * Affiche le type du vehicule
       21400-DISPLAY-TYPE.
           MOVE TYPE-TEMP TO TYPE-F
           DISPLAY TYPE-FORMAT
           .
      * Affiche le Nom-Prenom et la prime de base
       21500-DISPLAY-MONTANT.
           MOVE NOM-PRENOM TO NOM-PRENOM-F
           MOVE PRIME-DE-BASE TO MONTANT-F
           DISPLAY MONTANT-FORMAT
           .
      * Affiche le total general
       31000-DISPLAY-TOT-GEN.
           MOVE 'TOTAL GENERAL' TO TOT-N
           MOVE TOT-GEN TO TOT-F
           DISPLAY TOT-FORMAT
           .
      * Affiche le total du departement
       21110-DISPLAY-TOT-DEPT.
           MOVE 'TOTAL DEPARTEMENT' TO TOT-N
           MOVE TOT-DEPT TO TOT-F
           DISPLAY TOT-FORMAT
           DISPLAY HALF-LIGNE
           .
      * Affiche le total du type de vehicule
       21210-DISPLAY-TOT-TYPE.
           MOVE 'TOTAL DU TYPE' TO TOT-N
           MOVE TOT-TYPE TO TOT-F
           DISPLAY TOT-FORMAT
           DISPLAY HALF-LIGNE
           .
      * Gere le changement de departement
       21100-CHANGE-DEPT.
           ADD TOT-TYPE TO TOT-DEPT
           ADD TOT-DEPT TO TOT-GEN
           PERFORM 21210-DISPLAY-TOT-TYPE
           INITIALIZE TOT-TYPE
           PERFORM 21110-DISPLAY-TOT-DEPT
           INITIALIZE TOT-DEPT
           DISPLAY SPACE
           PERFORM 21300-DISPLAY-DEPT
           MOVE DEPARTEMENT TO DEPT-TEMP
           PERFORM 21400-DISPLAY-TYPE
           MOVE TYPE-VEHICULE TO TYPE-TEMP
           DISPLAY MONTANT-HEADER-FORMAT
           PERFORM 21500-DISPLAY-MONTANT
           PERFORM 21600-ADD-MONTANT
           .
      * Gere le changement du type de vehicule
       21200-CHANGE-TYPE.
           ADD TOT-TYPE TO TOT-DEPT
           PERFORM 21210-DISPLAY-TOT-TYPE
           INITIALIZE TOT-TYPE
           PERFORM 21400-DISPLAY-TYPE
           MOVE TYPE-VEHICULE TO TYPE-TEMP
           DISPLAY MONTANT-HEADER-FORMAT
           PERFORM 21500-DISPLAY-MONTANT
           PERFORM 21600-ADD-MONTANT
           .
      * Ajoute la prime de base au total du type de vehicule
       21600-ADD-MONTANT.
           ADD PRIME-DE-BASE TO TOT-TYPE
           .
