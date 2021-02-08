       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB14.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT f-etudiant ASSIGN detud
            file status is CR-ETUDIANT.
      *********************************
      *    D A T A   D I V I S I O N
      *********************************
       DATA DIVISION.
       FILE SECTION.
       fd f-etudiant
           block contains 0
           recording mode F
           record contains 50
           data record is e-etudiant.
       01 e-etudiant.
         05 NOM-PRENOM PIC X(20).
         05 NOTES-COEFFS PIC X(30).
         05 TAB-NOTES-COEFFS redefines NOTES-COEFFS.
           07 NOTE-COEFF OCCURS 10.
             09 NOTE PIC 99.
             09 COEFF PIC 9.

       WORKING-STORAGE SECTION.
       01 CR-ETUDIANT PIC 99.
         88 OK-ETUDIANT value 0.
         88 EOF-ETUDIANT value 10.
       
       01 FORMAT-MOYENNE PIC Z9,99.

       01 TOTAUX.
         05 TOT-NOTE PIC 9(3).
         05 TOT-COEFF PIC 9(2).
         05 TOT-MOYENNE PIC 9(3)V9(2).
       
       01 MOYENNE.
         05 MIN-MOYENNE PIC 9(2)V9(2) VALUE 20.
         05 MAX-MOYENNE PIC 9(2)V9(2) VALUE 0.
         05 GEN-MOYENNE PIC 9(2)V9(2).
         05 MOYENNE-CALCUL PIC 9(2)V9(2).

       01 MOYENNE-F.
         05 MIN-MOYENNE-F PIC Z9,99.
         05 MAX-MOYENNE-F PIC Z9,99.
         05 GEN-MOYENNE-F PIC Z9,99.
         05 MOYENNE-CALCUL-F PIC Z9,99.

       77 CPT-ETUDIANT PIC 9(2).
       77 NOTE-CALCUL PIC 9(3).
       77 I PIC 9(2).

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           PERFORM 10000-INIT-PGM
           PERFORM 20000-TRAITEMENT
           PERFORM 30000-END-PGM
           .
       10000-INIT-PGM.
           OPEN INPUT f-etudiant
           perform 11000-READ-ETUDIANT
           .
       20000-TRAITEMENT.
           PERFORM UNTIL EOF-ETUDIANT
             PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               perform 21000-HANDLE-NOTE
             END-PERFORM
             perform 22000-HANDLE-MOYENNE
             perform 11000-READ-ETUDIANT
             INITIALIZE I
           end-perform
           .
           
       30000-END-PGM.
           close f-etudiant
           perform 31000-SHOW-MOYENNES
           stop run
           .
       11000-READ-ETUDIANT.
           read f-etudiant
           IF OK-ETUDIANT
             ADD 1 TO CPT-ETUDIANT
           end-if
           .
       21000-HANDLE-NOTE.
           COMPUTE NOTE-CALCUL = NOTE(I) * COEFF(I)
           ADD NOTE-CALCUL TO TOT-NOTE
           ADD COEFF(I) TO TOT-COEFF
           .
       22000-HANDLE-MOYENNE.
           COMPUTE MOYENNE-CALCUL = TOT-NOTE / TOT-COEFF
           IF (MOYENNE-CALCUL > MAX-MOYENNE)
             MOVE MOYENNE-CALCUL TO MAX-MOYENNE 
           END-IF
           IF (MOYENNE-CALCUL < MIN-MOYENNE)
             MOVE MOYENNE-CALCUL TO MIN-MOYENNE
           END-IF
           ADD MOYENNE-CALCUL TO TOT-MOYENNE
           MOVE MOYENNE-CALCUL TO MOYENNE-CALCUL-F
           DISPLAY NOM-PRENOM "=> MOYENNE : " MOYENNE-CALCUL-F
           INITIALIZE TOT-NOTE
           INITIALIZE TOT-COEFF
           .
       31000-SHOW-MOYENNES.
           COMPUTE GEN-MOYENNE = TOT-MOYENNE / CPT-ETUDIANT
           MOVE GEN-MOYENNE TO GEN-MOYENNE-F
           DISPLAY "MOYENNE GENERAL : " GEN-MOYENNE-F
           MOVE MAX-MOYENNE TO MAX-MOYENNE-F
           DISPLAY "MOYENNE MAX : " MAX-MOYENNE-F
           MOVE MIN-MOYENNE TO MIN-MOYENNE-F
           DISPLAY "MOYENNE MIN  : " MIN-MOYENNE-F
           .