       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB8.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-CODE ASSIGN
            DDCODE FILE STATUS IS CR-CODE.
      *********************************
      *    D A T A   D I V I S I O N
      *********************************
       DATA DIVISION.

       FILE SECTION.
       fd F-CODE
           BLOCK CONTAINS 0
           DATA RECORD IS E-CODE.
       01 E-CODE                   PIC X(80).

       WORKING-STORAGE SECTION.
       01 CR-CODE                  PIC 99.
      * Variable de gestion de fichier 
       77 EOF-TRUE                 PIC X VALUE "Y".
       77 EOF                      PIC X VALUE "F".
      * -------- Structure ASSURES -----
       01 W-CODE.
         05                        PIC X(6).
         05 SPECIAL-CHAR           PIC X(1).
         05 ZONE-A                 PIC X(4).
         05 ZONE-B                 PIC X(69).
      * ----------- Compteurs  ---------------
       01 CPT.
         05 CPT-LIGNE-PROG         PIC 9(4) VALUE 0.
         05 CPT-LIGNE-COMMENT      PIC 9(4) VALUE 0.
         05 CPT-LIGNE-VIDE         PIC 9(4) VALUE 0.
      * ---------- Pourcentages ----------------
       01 PERCENT.
         05 PERCENT-COMMENT        PIC 9(2)V9(2).
         05 PERCENT-VIDE           PIC 9(2)V9(2).
      * ------------ FORMAT --------------------------
       01 FORMAT-HEADER.
         05                        PIC X(10).
         05                        PIC X(70) 
          VALUE 'Statistique sur les programmes'.
       01 FORMAT-LIGNE.
         05                        PIC X(10).
         05                        PIC X(3) VALUE ' - '.
         05 LIGNE-N                PIC X(30).
         05                        PIC X(3) VALUE ' : '.
         05 CPT-LIGNE-N            PIC 9(4).
       01 FORMAT-PERCENT.
         05                        PIC X(10).
         05                        PIC X(3) VALUE ' - '.
         05 PERCENT-N              PIC X(30).
         05                        PIC X(3) VALUE ' : '.
         05 PERCENT-EDIT           PIC Z9,99.
         05                        PIC X(1) VALUE '%'.
      * ------------------------------------------------ 
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
           OPEN INPUT F-CODE
           PERFORM 11000-DISPLAY-HEADER
           .
      * Parcours le fichier
       20000-TRAITEMENT.
           PERFORM UNTIL EOF = EOF-TRUE
             READ F-CODE INTO W-CODE
               AT END
                 MOVE EOF-TRUE TO EOF
               NOT AT END
                 PERFORM 21000-COUNT-LIGNE
             END-READ
           END-PERFORM
           .
      * Ferme le fichier
       30000-FIN.
           CLOSE F-CODE
           PERFORM 31000-CALCUL-PERCENT
           PERFORM 32000-DISPLAY-STATS
           .
      * Compte les lignes ainsi que les lignes commentes et vide
       21000-COUNT-LIGNE.
           ADD 1 TO CPT-LIGNE-PROG
           IF W-CODE = SPACE
             ADD 1 TO CPT-LIGNE-VIDE
           END-IF
           IF SPECIAL-CHAR = '*'
             ADD 1 TO CPT-LIGNE-COMMENT
           END-IF
           .
      * Calul des pourcentages de ligne commentes et vide
       31000-CALCUL-PERCENT.
           COMPUTE PERCENT-COMMENT =
            CPT-LIGNE-COMMENT * 100 / CPT-LIGNE-PROG
           COMPUTE PERCENT-VIDE =
            CPT-LIGNE-VIDE * 100 / CPT-LIGNE-PROG
           .
      * Affiche les informations en haut de page
       11000-DISPLAY-HEADER.
           DISPLAY FORMAT-HEADER
           .
       32000-DISPLAY-STATS SECTION.
      *  Gere l'affiche des stats de ligne
        32100-DISPLAY-STATS-LIGNE.
           MOVE 'Nombre de lignes du programme' TO LIGNE-N
           MOVE CPT-LIGNE-PROG TO CPT-LIGNE-N
           DISPLAY FORMAT-LIGNE
           MOVE 'Nombre de ligne en commentaires' TO LIGNE-N
           MOVE CPT-LIGNE-COMMENT TO CPT-LIGNE-N
           DISPLAY FORMAT-LIGNE
           MOVE 'Nombre de ligne vides' TO LIGNE-N
           MOVE CPT-LIGNE-VIDE TO CPT-LIGNE-N
           DISPLAY FORMAT-LIGNE
           .
      * Gere l'affichage des stats de pourcentage
        32200-DISPLAY-STATS-PERCENT.
           MOVE 'Pourcentages de commentaires' TO PERCENT-N
           MOVE PERCENT-COMMENT TO PERCENT-EDIT
           DISPLAY FORMAT-PERCENT
           MOVE 'Pourcentage de lignes blanches' TO PERCENT-N
           MOVE PERCENT-VIDE TO PERCENT-EDIT
           DISPLAY FORMAT-PERCENT
           .
