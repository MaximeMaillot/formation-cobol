       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB1.
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
           recording mode F
           DATA RECORD IS E-ASSURES.
       01  E-ASSURES.
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
         05 PIC X(9).

       WORKING-STORAGE SECTION.
       
       01  CR-ASSURES             PIC 99.

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           PERFORM 10000-INIT-PGM
           PERFORM 20000-TRAITEMENT
           PERFORM 30000-END-PGM
           .

       10000-INIT-PGM.
           OPEN INPUT F-ASSURES
           .
       20000-TRAITEMENT.
           READ F-ASSURES
           DISPLAY E-ASSURES
           .
       30000-END-PGM.
           CLOSE F-ASSURES
           STOP RUN
           .
