       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB9.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT f-stock ASSIGN ddstock.
           select f-compta assign ddcompta.
           select f-depassement assign dddepass.
           select f-stats assign ddstats.
      *********************************
      *    D A T A   D I V I S I O N
      *********************************
       DATA DIVISION.
       FILE SECTION.
       fd F-STOCK
           BLOCK CONTAINS 0
           DATA RECORD IS E-STOCK.
       01 E-STOCK.
         05 NUM-STOCK              PIC 9(6).
         05 NB-PRODUIT             PIC 9(4).
         05 PU-PRODUIT             PIC 9(4).
         05 NB-MIN                 PIC 9(4).
         05                        PIC X(62).       
       fd F-COMPTA.
       01 COMPTA                   PIC X(80).
       fd F-DEPASSEMENT.
       01 DEPASSEMENT              PIC X(80).
       fd F-STATS.
       01 STATS                    PIC X(80).

       WORKING-STORAGE SECTION.
      * ------------- Compteurs ----------- 
       01 CPT.
         05 CPT-STOCK              PIC 9(4) VALUE 0.
         05 CPT-COMPTA             PIC 9(4) VALUE 0.
         05 CPT-DEPASSEMENT        PIC 9(4) VALUE 0.
         05 CPT-ANOMALIE           PIC 9(4) VALUE 0.
      * ------------ Format ----------------- 
       01 FORMAT-COMPTA.
         05 NUM-STOCK-F            PIC 9(6).
         05                        PIC X(10).
         05 PRIX-TOT-F             PIC Z(5)9.
       01 FORMAT-STATS.
         05                        PIC X(43) VALUE 
           'Nombre d’enregistrement lus dans fichier '.
         05 FICHIER-N              PIC X(12).
         05                        PIC X(3) VALUE ' : '.
         05 CPT-F                  PIC Z(3)9.
      * Variable de gestion de fin de fichier 
       77 EOF-TRUE                 PIC X VALUE "Y".
       77 EOF                      PIC X VALUE "F".

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           PERFORM 10000-INIT-PGM
           PERFORM 20000-TRAITEMENT
           PERFORM 30000-END-PGM
           STOP RUN.
      * Ouvre stock, compta et depassement
       10000-INIT-PGM.
           OPEN INPUT F-STOCK
           OPEN OUTPUT F-COMPTA
           OPEN OUTPUT F-DEPASSEMENT
           .
      * Parcours stock
       20000-TRAITEMENT.
           PERFORM UNTIL EOF = EOF-TRUE
             READ F-STOCK
              AT END
                MOVE EOF-TRUE TO EOF
              NOT AT END
                ADD 1 TO CPT-STOCK
                PERFORM 21000-COMPTA
                PERFORM 22000-ALERTE
                PERFORM 23000-DEPASSEMENT
             END-READ
           END-PERFORM
           .
      * Gere le fichier compta
       21000-COMPTA.
           MOVE NUM-STOCK TO NUM-STOCK-F
           COMPUTE PRIX-TOT-F = NB-PRODUIT * PU-PRODUIT
           MOVE FORMAT-COMPTA TO COMPTA
           WRITE COMPTA
           ADD 1 TO CPT-COMPTA
           .
      * Affiche une alerte en cas de produit insuffisant
       22000-ALERTE.
           IF (NB-PRODUIT < NB-MIN)
             DISPLAY "Réapprovisionnement du produit N° " NUM-STOCK
             ADD 1 TO CPT-ANOMALIE
           .
      * Gere le fichier depassement
       23000-DEPASSEMENT.
           IF (NB-PRODUIT >= NB-MIN AND PU-PRODUIT > 150)
             MOVE NUM-STOCK TO NUM-STOCK-F
             COMPUTE PRIX-TOT-F = NB-PRODUIT * PU-PRODUIT
             MOVE FORMAT-COMPTA TO depassement
             WRITE DEPASSEMENT
             ADD 1 TO CPT-DEPASSEMENT
           .
      * Affiche les stats et ferme les fichiers
       30000-END-PGM.
           PERFORM 31000-STATS
           CLOSE F-STOCK
           CLOSE f-compta
           CLOSE f-depassement
           .
      * Gere le fichier stats
       31000-STATS.
           OPEN OUTPUT F-STATS
      * Stock
           MOVE 'STOCK' TO FICHIER-N
           MOVE CPT-STOCK TO CPT-F
           PERFORM 31100-WRITE-STATS
      * Compta
           MOVE 'COMPTA' TO FICHIER-N
           MOVE CPT-COMPTA TO CPT-F
           PERFORM 31100-WRITE-STATS
      * Depassement
           MOVE 'DEPASSEMENT' TO FICHIER-N
           MOVE CPT-DEPASSEMENT TO CPT-F
           PERFORM 31100-WRITE-STATS
      * Anomalie
           MOVE 'ANOMALIE' TO FICHIER-N
           MOVE CPT-ANOMALIE TO CPT-F
           PERFORM 31100-WRITE-STATS
      *     
           CLOSE F-STATS
           .
      * Ecrit dans stats     
       31100-WRITE-STATS.
           MOVE FORMAT-STATS TO STATS
           WRITE STATS
           .