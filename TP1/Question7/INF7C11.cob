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
           DATA RECORD IS E-ASSURES.
       01 E-ASSURES                PIC X(80).

       WORKING-STORAGE SECTION.
       01 CR-ASSURES               PIC 99.
       77 EOF-TRUE                 PIC X VALUE "Y".
       77 EOF                      PIC X VALUE "F".
      * -------- Structure ASSURES ----- 
       01 W-ASSURES.
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

      * ------------ Totaux --------- 
       77 TOT-TYPE                 PIC 9(5)V9(2) VALUE 0.     
       77 TOT-DEPT                 PIC 9(5)V9(2) VALUE 0.
       77 TOT-GEN                  PIC 9(5)V9(2) VALUE 0.

      * ----------- Variable temporaire --------- 
       77 TYPE-TEMP                PIC X(1) VALUE '1'.
       77 DEPT-TEMP                PIC 9(2) VALUE 00.

      * --------------- FORMAT --------------
       01 HEADER-FORMAT.
           05 PIC X(15).
           05 PIC X(42) VALUE 'Statistique par departement'.
           05 PIC X(15).
       01 MONTANT-HEADER-FORMAT.
           05 PIC X(10).
           05 PIC X(20) VALUE 'NOM / PRENOM'.
           05 PIC X(30).
           05 PIC X(12) VALUE 'MONTANT'.
       01 DEPT-FORMAT.
           05 PIC X(15) VALUE 'DEPARTEMENT'.
           05 PIC X(1) VALUE ':'.
           05 PIC X(5).
           05 DEPT-F PIC 9(2).
       01 TYPE-FORMAT.
           05 PIC X(15) VALUE 'TYPE'.
           05 PIC X(1) VALUE ':'.
           05 PIC X(5).
           05 TYPE-F PIC X(1).
       01 MONTANT-FORMAT.
           05 PIC X(10).
           05 NOM-PRENOM-F PIC X(20).
           05 PIC X(30).
           05 MONTANT-F PIC Z(5)9.99.
       01 TOT-FORMAT.
           05 TOT-N PIC X(20).
           05 PIC X(1) VALUE ':'.
           05 PIC X(39).
           05 TOT-F PIC Z(5)9.99.
       77 LIGNE PIC X(80) VALUE ALL '-'.
       01 HALF-LIGNE.
           05 PIC X(10).
           05 PIC X(10) VALUE '----------'.    

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           OPEN INPUT F-ASSURES
           PERFORM CONSTRUCT-HEADER
           PERFORM UNTIL EOF = EOF-TRUE
             READ F-ASSURES INTO W-ASSURES
               AT END
                 MOVE EOF-TRUE TO EOF
               NOT AT END
                 PERFORM ASSURES-L 
             END-READ
           END-PERFORM
           CLOSE F-ASSURES
           PERFORM DISPLAY-TOT-TYPE
           ADD TOT-TYPE TO TOT-DEPT
           ADD TOT-DEPT TO TOT-GEN
           PERFORM DISPLAY-TOT-DEPT
           PERFORM DISPLAY-TOT-GEN
           STOP RUN.
       CONSTRUCT-HEADER.
           DISPLAY HEADER-FORMAT
           DISPLAY SPACE
           DISPLAY LIGNE
           DISPLAY SPACE
           .
       ASSURES-L.
      *    Si on ne change pas de departement ni de type de vehicule
           IF DEPT-TEMP = DEPARTEMENT AND TYPE-TEMP = TYPE-VEHICULE
             PERFORM DISPLAY-MONTANT
             PERFORM ADD-MONTANT
           END-IF
      *    Si on ne change pas de departement 
      *    mais on change de type de vehicule     
           IF DEPT-TEMP = DEPARTEMENT AND TYPE-TEMP NOT = TYPE-VEHICULE
             PERFORM CHANGE-TYPE
           END-IF
      *    Si c'est la 1ère boucle
           IF TYPE-TEMP = '1' AND DEPT-TEMP = 00
             PERFORM DISPLAY-DEPT
             PERFORM DISPLAY-TYPE
             DISPLAY MONTANT-HEADER-FORMAT
             PERFORM DISPLAY-MONTANT
             PERFORM ADD-MONTANT
             MOVE DEPARTEMENT TO DEPT-TEMP
             MOVE TYPE-VEHICULE TO TYPE-TEMP
           END-IF
      *    Si on change de departement     
           IF DEPT-TEMP NOT = DEPARTEMENT
             PERFORM CHANGE-DEPT
           END-IF             
           .
       DISPLAY-DEPT.
           MOVE DEPARTEMENT TO DEPT-F
           DISPLAY DEPT-FORMAT 
           .
       DISPLAY-TYPE.
           MOVE TYPE-VEHICULE TO TYPE-F
           DISPLAY TYPE-FORMAT 
           .
       DISPLAY-MONTANT.
           MOVE NOM-PRENOM TO NOM-PRENOM-F
           MOVE PRIME-DE-BASE TO MONTANT-F
           DISPLAY MONTANT-FORMAT
           .
       DISPLAY-TOT-GEN.
           MOVE 'TOTAL GENERAL' TO TOT-N
           MOVE TOT-GEN TO TOT-F
           DISPLAY TOT-FORMAT
           .
       DISPLAY-TOT-DEPT.
           MOVE 'TOTAL DEPARTEMENT' TO TOT-N
           MOVE TOT-DEPT TO TOT-F
           DISPLAY TOT-FORMAT
           DISPLAY HALF-LIGNE
           .
       DISPLAY-TOT-TYPE.
           MOVE 'TOTAL DU TYPE' TO TOT-N
           MOVE TOT-TYPE TO TOT-F
           DISPLAY TOT-FORMAT
           DISPLAY HALF-LIGNE
           .
       CHANGE-DEPT.
           ADD TOT-TYPE TO TOT-DEPT
           ADD TOT-DEPT TO TOT-GEN
           PERFORM DISPLAY-TOT-TYPE
           INITIALIZE TOT-TYPE
           PERFORM DISPLAY-TOT-DEPT
           INITIALIZE TOT-DEPT
           DISPLAY SPACE
           PERFORM DISPLAY-DEPT
           MOVE DEPARTEMENT TO DEPT-TEMP
           PERFORM DISPLAY-TYPE
           MOVE TYPE-VEHICULE TO TYPE-TEMP
           DISPLAY MONTANT-HEADER-FORMAT
           PERFORM DISPLAY-MONTANT
           PERFORM ADD-MONTANT
           .
       CHANGE-TYPE.
           ADD TOT-TYPE TO TOT-DEPT
           PERFORM DISPLAY-TOT-TYPE
           INITIALIZE TOT-TYPE
           PERFORM DISPLAY-TYPE
           MOVE TYPE-VEHICULE TO TYPE-TEMP
           DISPLAY MONTANT-HEADER-FORMAT
           PERFORM DISPLAY-MONTANT
           PERFORM ADD-MONTANT
           .
       ADD-MONTANT.
           ADD PRIME-DE-BASE TO TOT-TYPE
           .
