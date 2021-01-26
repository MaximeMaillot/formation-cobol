       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB2.
      ****************************************************************
      *    D A T A   D I V I S I O N
      ***********************************************************
       DATA DIVISION.
      ****************************************************************
      *    W O R K I N G - S T O R A G E   S E C T I O N
      ****************************************************************
       WORKING-STORAGE SECTION.
      *--------------DEFINITION DES VARIABLES---------------------
       01  ASSURES.
           05  MATRICULE          PIC 9(6).
           05  NOM-PRENOM         PIC X(20).
           05  ADRESSE.
               10  RUE-ADRESSE    PIC X(18).
               10  CODE-POSTAL    PIC 9(5).
               10  VILLE          PIC X(12).
           05  ASSURANCE.
               10  TYPE-VEHICULE  PIC X(1).
               10  PRIME-DE-BASE  PIC 9(4)V9(2).
               10  BONUS-MALUS    PIC X(1).
               10  TAUX           PIC 9(2).

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           ACCEPT ASSURES
           PERFORM ASSURES-L UNTIL MATRICULE = 999999.
           STOP RUN.
      * PARCOURS ASSURES
       ASSURES-L.
      * Affichage tant que le matricule n'est pas égal à 999999
           IF MATRICULE NOT EQUAL 999999
               DISPLAY "MATRICULE : " MATRICULE
               DISPLAY "NOM PRENOM : " NOM-PRENOM
               DISPLAY "RUE ADRESSE : " RUE-ADRESSE
               DISPLAY "CODE POSTAL : " CODE-POSTAL
               DISPLAY "VILLE : " VILLE
               DISPLAY "TYPE VEHICULE : " TYPE-VEHICULE
               DISPLAY "PRIME DE BASE : " PRIME-DE-BASE
               DISPLAY "BONUS MALUS : " BONUS-MALUS
               DISPLAY "TAUX : " TAUX
               DISPLAY "----------------------------------------------"
               ACCEPT ASSURES
           END-IF.