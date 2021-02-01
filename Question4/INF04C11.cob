       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB4.
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
           05 PIC X(14).
           
       WORKING-STORAGE SECTION.
       01  CR-ASSURES             PIC 99.
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
           05 MATRICULE-F PIC X(10).
           05 PIC X(10).
           05 DATE-F PIC X(30).
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

       77  EOF-TRUE                PIC X VALUE "Y".
       77  EOF                     PIC X VALUE "F".

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           PERFORM INIT-VAR
           OPEN INPUT F-ASSURES
           PERFORM UNTIL EOF = EOF-TRUE
             READ F-ASSURES
                AT END
                    MOVE EOF-TRUE TO EOF
                NOT AT END
                   PERFORM HEADER-F
                   PERFORM QUITTANCE-F
                   DISPLAY LIGNE
             END-READ
           END-PERFORM
           CLOSE F-ASSURES
           STOP RUN.
       INIT-VAR.
           ACCEPT DATE-N FROM DATE YYYYMMDD
           ACCEPT TIME-N FROM TIME
           MOVE "F" TO EOF
           .
       HEADER-F.
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
           MOVE MATRICULE TO MATRICULE-F
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
           DISPLAY FORMAT-IDENTITE
           .
       QUITTANCE-F.
      * Calcul la quittance de prime
           MOVE "PRIME DE BASE" TO NOM-FACTURE
           MOVE PRIME-DE-BASE TO PRIX-FACTURE
           DISPLAY FORMAT-FACTURE
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
           INITIALIZE TAUX-B
           INITIALIZE TYPE-B
           MOVE PRIX-TOTAL-C TO PRIX-FACTURE
           MOVE "TOTAL A PAYER" TO NOM-FACTURE
           DISPLAY FORMAT-FACTURE
           .