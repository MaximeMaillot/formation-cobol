       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB11.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT f-region ASSIGN dregion
            file status is CR-REGION.

           SELECT f-mvt ASSIGN dmvt
            file status is CR-MVT.

           SELECT f-recap ASSIGN drecap
            file status is CR-RECAP.
            
           SELECT f-stats assign dstatsr
            file status is CR-STATS.
      *********************************
      *    D A T A   D I V I S I O N
      *********************************
       DATA DIVISION.
       FILE SECTION.
       fd f-region
           BLOCK CONTAINS 0
           record contains 80
           recording mode F
           DATA RECORD IS e-region.
       01 e-region.
         05 MAT-REGION PIC 9(3).
         05 CODE-REGION PIC X(2).
         05 NUM-REGION-REGION PIC 9(2).
         05 NOM-REGION PIC X(20).
         05            PIC X(53).

       fd f-mvt
           BLOCK CONTAINS 0
           record contains 80
           recording mode F
           DATA RECORD IS e-mvt.
       01 e-mvt.
         05 MAT-MVT PIC 9(3).
         05 CODE-MVT PIC X(2).
         05 NUM-REGION-MVT PIC 9(2).
         05 CODE-AGENCE PIC 9(2).
         05 CODE-VENDEUR PIC 9(2).
         05 NB-DOSSIER PIC 9(4).
         05 PIC X(65).

       fd f-recap.
       01 recap                    PIC X(80).
       
       fd f-stats.
       01 stats                    PIC X(80).

       WORKING-STORAGE SECTION.
       01 CR-REGION PIC 99.
         88 EOF-REGION VALUE 10.
       01 CR-MVT PIC 99.
         88 EOF-MVT VALUE 10.
       01 CR-RECAP PIC 99.
       01 CR-STATS PIC 99.

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
         05 NUM-REGION-RECAP PIC 9(2).
         05 CORPS-ENRGT PIC X(72).
         05 CORPS-ENRGT-400 redefines CORPS-ENRGT.
           10 CODE-AGENCE-400 PIC 9(2).
           10 NB-DOSSIER-400 PIC 9(5).
           10 PIC X(65).
         05 CORPS-ENRGT-500 redefines CORPS-ENRGT.
           10 CODE-AGENCE-500 PIC 9(2).
           10 NOM-REGION-500 PIC X(20).
           10 NB-DOSSIER-500 PIC 9(5).
           10 PIC X(45).

      * ---------- Variable de calcul -------------
       01 TOTAUX.
         05 TOTAL-AGENCE PIC 9(5).
         05 TOTAL-REGION PIC 9(5).     

      * -------- Stock le dernier parcours d'un fichier ------- 
       01 PREV.
         05 PREV-REGION.
           10 PREV-NUM-REGION-REGION PIC 9(2).
         05 PREV-MVT.
           10 PREV-NUM-REGION-MVT PIC 9(2).
           10 PREV-CODE-AGENCE PIC 9(2).
           10 PREV-CODE-VENDEUR PIC 9(2).

      * Variable permettant de boucler sur les agences 
       77 CODE-AGENCE-TEMP PIC 9(2).

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           PERFORM 10000-INIT-PGM
           PERFORM 20000-TRAITEMENT
           PERFORM 30000-END-PGM
           .
           
      * Lit le fichier et gere les erreurs
       11000-READ-REGION.
           MOVE NUM-REGION-REGION TO PREV-NUM-REGION-REGION
           READ f-region
             NOT AT END
               perform 11100-CHECK-ERROR-REGION
               ADD 1 TO CPT-MAT-REGION
           END-READ
           .

      * Lit le fichier mvt et gere les erreurs
       12000-READ-MVT.
           MOVE NUM-REGION-MVT TO PREV-NUM-REGION-MVT
           MOVE CODE-AGENCE TO PREV-CODE-AGENCE 
           MOVE CODE-VENDEUR  TO PREV-CODE-VENDEUR
           READ f-mvt
             NOT AT END
               perform 12100-CHECK-ERROR-MVT
               ADD 1 TO CPT-MAT-MVT
           END-READ
           .

      * Gestion d'erreur du fichier region
       11100-CHECK-ERROR-REGION.
      *    Erreur d'immatriculation
           IF (MAT-REGION NOT = CPT-MAT-REGION)
             perform 30000-END-PGM         
           END-IF
      *    Erreur de tri     
           IF (PREV-NUM-REGION-REGION > NUM-REGION-REGION)
             perform 30000-END-PGM
           END-IF
           .

      * Gestion d'erreur du fichier MVT
       12100-CHECK-ERROR-MVT.
      *    Erreur d'immatriculation
           IF (MAT-MVT NOT = CPT-MAT-MVT)
             perform 30000-END-PGM
           END-IF
      *    Erreurs de tri
           IF (PREV-NUM-REGION-MVT > NUM-REGION-MVT)
             perform 30000-END-PGM 
           end-if
           IF (PREV-CODE-AGENCE > CODE-AGENCE)
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
           PERFORM 11000-READ-REGION
           PERFORM 12000-READ-MVT
           .

      * Parcours les fichiers
       20000-TRAITEMENT.
           perform until EOF-MVT
      *      Cas normal
             IF NUM-REGION-MVT = NUM-REGION-REGION
      *        Parcours une region         
               perform until NUM-REGION-MVT NOT = NUM-REGION-REGION
                 OR EOF-MVT
                 MOVE CODE-AGENCE TO CODE-AGENCE-TEMP
      *          Parcours les agences d'une region
                 perform until CODE-AGENCE NOT = CODE-AGENCE-TEMP
                   perform NO-CHANGE
                 END-PERFORM
                 perform 21000-WRITE-CODE-400
               END-PERFORM
               perform 22000-WRITE-CODE-500
      *      Cas particuliers
             ELSE
               IF (NUM-REGION-MVT < NUM-REGION-REGION) OR EOF-REGION
                 perform 23000-WRITE-CODE-999
                 perform 12000-READ-MVT
               ELSE
                 perform 11000-READ-REGION
               END-IF
             END-IF
           END-PERFORM   
           .

       NO-CHANGE.
           ADD NB-DOSSIER TO TOTAL-AGENCE
           perform 12000-READ-MVT
           .

       21000-WRITE-CODE-400.
           MOVE '400' TO CODE-RECAP
           MOVE NUM-REGION-REGION TO NUM-REGION-RECAP
           MOVE CODE-AGENCE-TEMP TO CODE-AGENCE-400
           MOVE TOTAL-AGENCE TO NB-DOSSIER-400
           perform 21100-WRITE-RECAP
           ADD TOTAL-AGENCE TO TOTAL-REGION
           INITIALIZE TOTAL-AGENCE
           .

       22000-WRITE-CODE-500.
           MOVE '500' TO CODE-RECAP
           MOVE NUM-REGION-REGION TO NUM-REGION-RECAP
           MOVE 0 TO CODE-AGENCE-500
           MOVE NOM-REGION TO NOM-REGION-500
           MOVE TOTAL-REGION TO NB-DOSSIER-500
           perform 21100-WRITE-RECAP
           INITIALIZE TOTAL-REGION
           .

       23000-WRITE-CODE-999.
           MOVE '999' TO CODE-RECAP
           MOVE NUM-REGION-MVT TO NUM-REGION-RECAP
           perform 21100-WRITE-RECAP
           .

       21100-WRITE-RECAP.
           MOVE CPT-MAT-RECAP TO MAT-RECAP
           WRITE recap from FORMAT-RECAP
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