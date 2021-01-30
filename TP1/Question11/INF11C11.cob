       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB11.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT f-region ASSIGN dregion.
           SELECT f-mvt ASSIGN dmvt.
           SELECT f-recap ASSIGN drecap.
           SELECT f-stats assign dstatsr.
      *********************************
      *    D A T A   D I V I S I O N
      *********************************
       DATA DIVISION.
       FILE SECTION.
       fd f-region
           BLOCK CONTAINS 0
           DATA RECORD IS e-region.
       01 e-region.
         05 MAT-REGION PIC 9(3).
         05 CODE-REGION PIC X(2).
         05 NUM-REGION PIC 9(2).
         05 NOM-REGION PIC X(20).
         05            PIC X(53).

       fd f-mvt
           BLOCK CONTAINS 0
           DATA RECORD IS e-mvt.
       01 e-mvt.
         05 MAT-MVT PIC 9(3).
         05 CODE-MVT PIC X(2).
         05 NUM-REGION PIC 9(2).
         05 CODE-AGENCE PIC 9(2).
         05 CODE-VENDEUR PIC 9(2).
         05 NB-DOSSIER PIC 9(4).
         05 PIC X(65).
       fd f-recap.
       01 recap                    PIC X(80).
       fd f-stats.
       01 stats                    PIC X(80).

       WORKING-STORAGE SECTION.
      * --------------- Compteurs ----------- 
       01 CPT.
         05 CPT-CHECK.
           10 CPT-MAT-REGION PIC 9(3) VALUE 100.
           10 CPT-MAT-MVT PIC 9(3) VALUE 100.
         05 CPT-FORMAT.
           10 CPT-MAT-RECAP PIC 9(2) VALUE 1.


      * ---------------- Format -------------- 
       01 FORMAT-RECAP.
         05 MAT-RECAP PIC 9(3).
         05 CODE-RECAP PIC 9(3).
         05 NUM-REGION PIC 9(2).
         05 CORPS-ENRGT PIC X(72).
         05 CORPS-ENRGT-400 redefines CORPS-ENRGT.
           10 CODE-AGENCE PIC 9(2).
           10 NB-DOSSIER PIC 9(5).
           10 PIC X(65).
         05 CORPS-ENRGT-500 redefines CORPS-ENRGT.
           10 CODE-AGENCE PIC 9(2).
           10 NOM-REGION PIC X(20).
           10 NB-DOSSIER PIC 9(5).
           10 PIC X(45).

      * ---------- Variable de calcul -------------
       01 TOTAUX.
         05 TOTAL-AGENCE PIC 9(5).
         05 TOTAL-REGION PIC 9(5).     

      * -------- Stock le dernier parcours d'un fichier ------- 
       01 PREV.
         05 PREV-REGION.
           10 PREV-NUM-REGION PIC 9(2).
         05 PREV-MVT.
           10 PREV-NUM-REGION PIC 9(2).
           10 PREV-CODE-AGENCE PIC 9(2).
           10 PREV-CODE-VENDEUR PIC 9(2).

      * Variable permettant de boucler sur les agences 
       77 CODE-AGENCE-TEMP PIC 9(2).

      * Variables de fin de fichier 
       77 EOF-TRUE                 PIC X VALUE "Y".
       77 EOF-REGION               PIC X VALUE "F".
       77 EOF-MVT                  PIC X VALUE "F".

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           PERFORM 10000-INIT-PGM
           PERFORM 20000-TRAITEMENT
           PERFORM 30000-END-PGM
           .
      * Lit le fichier et gere les erreurs
       READ-REGION.
           READ f-region
             AT END
               MOVE EOF-TRUE TO EOF-REGION
             NOT AT END
               perform CHECK-ERROR-REGION
               ADD 1 TO CPT-MAT-REGION
               MOVE NUM-REGION of e-region 
                TO PREV-NUM-REGION of PREV-REGION
           END-READ
           .
      * Lit le fichier mvt et gere les erreurs
       READ-MVT.
           READ f-mvt
             AT END
               MOVE EOF-TRUE TO EOF-MVT
             NOT AT END
               perform CHECK-ERROR-MVT
               ADD 1 TO CPT-MAT-MVT
               MOVE NUM-REGION of e-mvt 
                TO PREV-NUM-REGION of PREV-MVT
               MOVE CODE-AGENCE of e-mvt TO PREV-CODE-AGENCE
               MOVE CODE-VENDEUR of e-mvt TO PREV-CODE-VENDEUR
           END-READ
           .
      * Gestion d'erreur du fichier region
       CHECK-ERROR-REGION.
      *    Erreur d'immatriculation
           IF (MAT-REGION NOT = CPT-MAT-REGION)
             perform 30000-END-PGM         
           END-IF
      *    Erreur de tri     
           IF (PREV-NUM-REGION of PREV-REGION > NUM-REGION of e-region)
             perform 30000-END-PGM
           END-IF
           .
      * Gestion d'erreur du fichier MVT
       CHECK-ERROR-MVT.
      *    Erreur d'immatriculation
           IF (MAT-MVT NOT = CPT-MAT-MVT)
             perform 30000-END-PGM
           END-IF
      *    Erreurs de tri
           IF (PREV-NUM-REGION of PREV-MVT > NUM-REGION of e-mvt)
             perform 30000-END-PGM 
           end-if
           IF (PREV-CODE-AGENCE > CODE-AGENCE of e-mvt)
             perform 30000-END-PGM 
           end-if
           IF (PREV-CODE-VENDEUR > CODE-VENDEUR)
             perform 30000-END-PGM 
           end-if
           .
      * Ouvre les fichiers et effectue la première lecture
       10000-INIT-PGM. 
           OPEN INPUT f-region
           OPEN INPUT f-mvt
           OPEN OUTPUT f-recap
           PERFORM READ-REGION
           PERFORM READ-MVT
           MOVE CODE-AGENCE of e-mvt TO CODE-AGENCE-TEMP
           .
      * Parcours les fichiers
       20000-TRAITEMENT. 
           perform until EOF-REGION = EOF-TRUE     
             IF NUM-REGION of e-region NOT = NUM-REGION of e-mvt
               perform 23000-WRITE-CODE-999
             ELSE
      *        Tant que la region de f-mvt et de f-region est similaire           
               perform until 
                (EOF-MVT = EOF-TRUE) OR 
                (NUM-REGION of e-region NOT = NUM-REGION of e-mvt)
      *          Tant que le code d'agence ne change pas
                 perform until 
                  (EOF-MVT = EOF-TRUE) OR
                  (CODE-AGENCE of e-mvt NOT = CODE-AGENCE-TEMP)
                   ADD NB-DOSSIER of e-mvt TO TOTAL-AGENCE
                   perform READ-MVT
                 end-perform
                 perform 21000-WRITE-CODE-400
                 ADD TOTAL-AGENCE TO TOTAL-REGION
                 MOVE CODE-AGENCE of e-mvt TO CODE-AGENCE-TEMP
                 INITIALIZE TOTAL-AGENCE
                 INITIALIZE PREV-CODE-VENDEUR
               end-perform
               perform 22000-WRITE-CODE-500
               INITIALIZE TOTAL-REGION
               INITIALIZE PREV-CODE-AGENCE
             END-IF
             perform READ-REGION
           end-perform
           .
       21000-WRITE-CODE-400.
           MOVE '400' TO CODE-RECAP
           MOVE NUM-REGION of e-region TO NUM-REGION of FORMAT-RECAP
           MOVE CODE-AGENCE-TEMP TO CODE-AGENCE of CORPS-ENRGT-400
           MOVE TOTAL-AGENCE TO NB-DOSSIER of CORPS-ENRGT-400
           perform WRITE-RECAP
           .
       22000-WRITE-CODE-500.
           MOVE '500' TO CODE-RECAP   
           MOVE NUM-REGION of e-region TO NUM-REGION of FORMAT-RECAP
      *    Code agence gere par le INITALIZE
           MOVE NOM-REGION of e-region TO NOM-REGION of CORPS-ENRGT-500
           MOVE TOTAL-REGION TO NB-DOSSIER of CORPS-ENRGT-500
           perform WRITE-RECAP
           .
       23000-WRITE-CODE-999.
           MOVE '999' TO CODE-RECAP
           MOVE NUM-REGION of e-region TO NUM-REGION of FORMAT-RECAP
           perform WRITE-RECAP
           .
       WRITE-RECAP.
           MOVE CPT-MAT-RECAP TO MAT-RECAP
           MOVE FORMAT-RECAP TO recap
           WRITE recap
           INITIALIZE FORMAT-RECAP
           ADD 1 TO CPT-MAT-RECAP
           .

      * Ferme les fichiers
       30000-END-PGM.
           PERFORM 31000-STATS
           CLOSE f-region
           CLOSE f-mvt
           CLOSE f-recap
           STOP RUN
           .
      * Gere le fichier stats
       31000-STATS.
           OPEN OUTPUT F-STATS
      *    TODO
      *    Faire les stats
           CLOSE F-STATS
           .