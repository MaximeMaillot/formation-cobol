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
              10  RUE-ADRESSE    PIC X(18).
              10  CODE-POSTAL    PIC 9(5).
              10  VILLE          PIC X(12).
          05  ASSURANCE.
              10  TYPE-VEHICULE  PIC X(1).
              10  PRIME-DE-BASE  PIC 9(4)V9(2).
              10  BONUS-MALUS    PIC X(1).
              10  TAUX           PIC 9(2).
       77  PRIX-BM               PIC 9(4)V9(2).
       77  PRIX-TOTAL            PIC 9(4)V9(2).
       77  PRIX-DISP             PIC Z(9)9.99.

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           ACCEPT ASSURES
      * BEFORE => EQUIVALENT DO WHILE
           PERFORM ASSURES-L WITH TEST BEFORE UNTIL MATRICULE = 999999.
           STOP RUN.
      * PARCOURS ASSURES
       ASSURES-L.
           DISPLAY "QUITTANCE DE PRIME"
           DISPLAY NOM-PRENOM
           DISPLAY RUE-ADRESSE
           DISPLAY CODE-POSTAL " / " VILLE
           MOVE PRIME-DE-BASE TO PRIX-DISP
           DISPLAY "PRIME DE BASE : " PRIX-DISP
           COMPUTE PRIX-BM = PRIME-DE-BASE * TAUX / 100
           DISPLAY "---"
           IF BONUS-MALUS = 'B'
              MOVE PRIX-BM TO PRIX-DISP
              DISPLAY "MAJORATION : " PRIX-DISP " / MALUS : " TAUX "%"
              COMPUTE PRIX-TOTAL = PRIME-DE-BASE + PRIX-BM
              MOVE PRIX-TOTAL TO PRIX-DISP
              DISPLAY "TOTAL A PAYER : " PRIX-DISP
           ELSE
              MOVE PRIX-BM TO PRIX-DISP
              DISPLAY "DEGREVEMENT : " PRIX-DISP " / BONUS : " TAUX "%"
              COMPUTE PRIX-TOTAL = PRIME-DE-BASE - PRIX-BM
              MOVE PRIX-TOTAL TO PRIX-DISP
              DISPLAY "TOTAL A PAYER : " PRIX-DISP
           END-IF.
           DISPLAY "------------------"
           ACCEPT ASSURES.
