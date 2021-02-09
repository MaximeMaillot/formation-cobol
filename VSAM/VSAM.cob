       IDENTIFICATION DIVISION.
       PROGRAM-ID. VSAM1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-ASSURES ASSIGN DDASSUR
            ORGANIZATION INDEXED
            ACCESS MODE RANDOM
            RECORD KEY MATRICULE
            FILE STATUS IS CR-ASSURES.
      *********************************
      *    D A T A   D I V I S I O N
      *********************************
       DATA DIVISION.
       FILE SECTION.
       fd IDX-ASSURES
           DATA RECORD IS E-ASSURES.
       01  E-ASSURES.
         05  MATRICULE.
           10 KEY-ASSURES PIC 9(6).
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

       01 ACCEPT-SYSIN PIC 9(6).
       
       01  CR-ASSURES             PIC 99.
         88 ASSURES-OK VALUE 0.
         88 ASSURES-DUPLICATE-KEY VALUE 22.
         88 ASSURES-KEY-NOT-FOUND VALUE 23.

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           PERFORM 10000-INIT-PGM
           PERFORM 20000-TRAITEMENT
           PERFORM 30000-END-PGM
           .

       10000-INIT-PGM.
           OPEN INPUT IDX-ASSURES
           ACCEPT ACCEPT-SYSIN
           .
       20000-TRAITEMENT.
           perform until ACCEPT-SYSIN = 999999
             MOVE ACCEPT-SYSIN TO MATRICULE
             perform 21000-READ-ASSURES
             ACCEPT ACCEPT-SYSIN
           END-PERFORM
           .
       30000-END-PGM.
           CLOSE IDX-ASSURES
           STOP RUN
           .
       21000-READ-ASSURES.
           READ IDX-ASSURES
           EVALUATE TRUE
             WHEN ASSURES-OK
               DISPLAY E-ASSURES
             WHEN ASSURES-DUPLICATE-KEY 
               DISPLAY "DUPLICATE KEY : " MATRICULE
             WHEN ASSURES-KEY-NOT-FOUND
               DISPLAY "KEY NOT FOUND : " MATRICULE
             WHEN OTHER 
               DISPLAY "ERROR : " MATRICULE 
           END-EVALUATE
           .
