       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB4.
      ****************************************************************
      *    D A T A   D I V I S I O N
      ***********************************************************
       DATA DIVISION.
      ****************************************************************
      *    W O R K I N G - S T O R A G E   S E C T I O N
      ****************************************************************
       WORKING-STORAGE SECTION.
      * Structure du fichier APIN.COB.ASSURES
       01  ASSURES.
           05  MATRICULE          PIC 9(6).
           05  NOM-PRENOM         PIC X(20).
           05  ADRESSE.
              10  RUE-ADRESSE     PIC X(18).
              10  CODE-POSTAL     PIC 9(5).
              10  VILLE           PIC X(12).
           05  ASSURANCE.
              10  TYPE-VEHICULE   PIC X(1).
              10  PRIME-DE-BASE   PIC 9(4)V9(2).
              10  BONUS-MALUS     PIC X(1).
              10  TAUX            PIC 9(2).
      * Structure pour recuperer la date       
       01  FORMAT-DATE.
           05  DATE-N.
               10 YEAR-N          PIC 9(4).
               10 MONTH-N         PIC 9(2).
               10 DAY-N           PIC 9(2).
           05  TIME-N.
               10 HOUR-N          PIC 9(2).
               10 MINUTE-N        PIC 9(2).
      * ---------------- FORMATTAGE ------------------------------------
      * Quittance de prime et date
       01  FORMAT-QUITTANCE.
           05 PIC X(1) VALUE '|'.
           05 PIC X(20) VALUE ' QUITTANCE DE PRIME'.
           05 PIC X(10).
           05 PIC X(1) VALUE '/'.
           05 PIC X(10).
           05 DATE-F PIC X(29).
           05 PIC X(1) VALUE '|'.
      * Nom, Adresse et Code Postal/Ville
       01  FORMAT-IDENTITE.
           05  PIC X(1) VALUE '|'.
           05  PIC X(15).
           05  PIC X(5) VALUE '|***|'.
           05  NOM-IDENTITE PIC X(20).
           05  PIC X(5) VALUE '|***|'.
           05  PIC X(25).
           05  PIC X(1) VALUE '|'.
      * Les prix
       01  FORMAT-FACTURE.
           05  PIC X(1) VALUE '|'.
           05  NOM-FACTURE PIC X(20).
      *    Variable de formatage des prix
           05  PRIX-FACTURE PIC Z(5)9.99.
           05  PIC X(20).
           05  TYPE-B PIC X(10).
           05  PIC X(5).
           05  TAUX-B PIC X(5).
           05  PIC X(1).
           05  PIC X(1) VALUE '|'.
      * ----------------------------------------------------------------         
      * Variables de calcul    
       77  PRIX-BM-C              PIC 9(4)V9(2).
       77  PRIX-TOTAL-C           PIC 9(5)V9(2).
      * Permet d'afficher une ligne de '-' 
       77  LIGNE PIC X(80) VALUE ALL '-'.

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           ACCEPT ASSURES
           ACCEPT DATE-N FROM DATE YYYYMMDD
           ACCEPT TIME-N FROM TIME

           PERFORM ASSURES-L UNTIL MATRICULE = 999999.
           STOP RUN.
      * PARCOURS ASSURES
       ASSURES-L.
      * Affiche les informations du client
      * STRING concatene dans une variable (ici Date-F)
      * DELIMITED BY SIZE : Concatene tout
      * DELIMITED BY SPACE : Concatene jusqu'à trouver un espace
           STRING "Le " DELIMITED BY SIZE
            DAY-N DELIMITED BY SIZE
            "/" DELIMITED BY SIZE
            MONTH-N DELIMITED BY SIZE
            "/" DELIMITED BY SIZE
            YEAR-N DELIMITED BY SIZE
            " a " DELIMITED BY SIZE
            HOUR-N DELIMITED BY SIZE
            ":" DELIMITED BY SIZE
            MINUTE-N DELIMITED BY SIZE
            INTO DATE-F
           END-STRING
           DISPLAY FORMAT-QUITTANCE
           MOVE NOM-PRENOM TO NOM-IDENTITE
           DISPLAY FORMAT-IDENTITE
           MOVE RUE-ADRESSE TO NOM-IDENTITE
           DISPLAY FORMAT-IDENTITE
           STRING CODE-POSTAL DELIMITED BY SIZE
            " / " DELIMITED BY SIZE
            VILLE DELIMITED BY SIZE
            INTO NOM-IDENTITE
           END-STRING
           DISPLAY FORMAT-IDENTITE.
      * Calcul la quittance de prime
           MOVE "PRIME DE BASE" TO NOM-FACTURE
           MOVE PRIME-DE-BASE TO PRIX-FACTURE
           DISPLAY FORMAT-FACTURE
      * COMPUTE permet de faire un calcul
           COMPUTE PRIX-BM-C = PRIME-DE-BASE * TAUX / 100
           IF BONUS-MALUS = 'M'
              MOVE PRIX-BM-C TO PRIX-FACTURE
              MOVE "MARJORATION" TO NOM-FACTURE
              MOVE "MALUS" TO TYPE-B
              STRING TAUX DELIMITED BY SIZE
               "%" DELIMITED BY SIZE
               INTO TAUX-B
              END-STRING
              DISPLAY FORMAT-FACTURE
              COMPUTE PRIX-TOTAL-C = PRIME-DE-BASE + PRIX-BM-C
           ELSE
              MOVE PRIX-BM-C TO PRIX-FACTURE
              MOVE "DEGREVEMENT" TO NOM-FACTURE
              MOVE "BONUS" TO TYPE-B
              STRING TAUX DELIMITED BY SIZE
               "%" DELIMITED BY SIZE
               INTO TAUX-B
              END-STRING
              DISPLAY FORMAT-FACTURE
              COMPUTE PRIX-TOTAL-C = PRIME-DE-BASE - PRIX-BM-C
           END-IF
      * INITIALIZE remet les numeriques à 0 et les chaînes en vide     
           INITIALIZE TAUX-B
           INITIALIZE TYPE-B
           MOVE PRIX-TOTAL-C TO PRIX-FACTURE
           MOVE "TOTAL A PAYER" TO NOM-FACTURE
           DISPLAY FORMAT-FACTURE
           DISPLAY LIGNE
      * Lire la prochaine ligne d'ASSURES
           ACCEPT ASSURES
           .