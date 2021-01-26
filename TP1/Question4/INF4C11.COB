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
      *--------------DEFINITION DES VARIABLES---------------------
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
       01  FORMAT-DATE.
           05  DATE-N.                 
               10 YEAR-N          PIC 9(4).
               10 MONTH-N         PIC 9(2).
               10 DAY-N           PIC 9(2).
           05  TIME-N.
               10 HOUR-N          PIC 9(2).
               10 MINUTE-N        PIC 9(2).
           05  DAYWEEK-N          PIC 9(1).
      *01  ARRAY-DAYWEEK OCCURES 7 TIMES INDEXED BY DAYWEEK-N.
      *    05 DAYWEEK             PIC X(10).      
       77  PRIX-BM-C              PIC 9(4)V9(2).
       77  PRIX-TOTAL-C           PIC 9(4)V9(2).
       77  PRIX-E                 PIC Z(9)9.99.     

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           ACCEPT ASSURES
           ACCEPT DATE-N FROM DATE YYYYMMDD
           ACCEPT TIME-N FROM TIME
           ACCEPT DAYWEEK-N FROM DAY-OF-WEEK
           
      * BEFORE => EQUIVALENT DO WHILE
           PERFORM ASSURES-L WITH TEST BEFORE UNTIL MATRICULE = 999999.
           STOP RUN.
      * PARCOURS ASSURES
       ASSURES-L.
           DISPLAY "QUITTANCE DE PRIME / Le "
            DAY-N "/" MONTH-N "/" YEAR-N " a " HOUR-N ":" MINUTE-N
      *     " Jour semaine : " DAYWEEK (DAYWEEK-N)    
           DISPLAY NOM-PRENOM
           DISPLAY RUE-ADRESSE
           DISPLAY CODE-POSTAL " / " VILLE
           MOVE PRIME-DE-BASE TO PRIX-E
           DISPLAY "PRIME DE BASE : " PRIX-E
           COMPUTE PRIX-BM-C = PRIME-DE-BASE * TAUX / 100
           DISPLAY "---"
           IF BONUS-MALUS = 'M'
              MOVE PRIX-BM-C TO PRIX-E
              DISPLAY "MAJORATION : " PRIX-E " / MALUS : " TAUX "%"
              COMPUTE PRIX-TOTAL-C = PRIME-DE-BASE + PRIX-BM-C
              MOVE PRIX-TOTAL-C TO PRIX-E
              DISPLAY "TOTAL A PAYER : " PRIX-E
           ELSE
              MOVE PRIX-BM-C TO PRIX-E
              DISPLAY "DEGREVEMENT : " PRIX-E " / BONUS : " TAUX "%"
              COMPUTE PRIX-TOTAL-C = PRIME-DE-BASE - PRIX-BM-C
              MOVE PRIX-TOTAL-C TO PRIX-E
              DISPLAY "TOTAL A PAYER : " PRIX-E
           END-IF.
           DISPLAY "------------------"
           ACCEPT ASSURES.
