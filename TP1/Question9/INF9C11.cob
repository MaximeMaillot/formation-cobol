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
      ****************
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
       01 CPT.
         05 CPT-STOCK              PIC 9(4) VALUE 0.
         05 CPT-COMPTA             PIC 9(4) VALUE 0.
         05 CPT-DEPASSEMENT        PIC 9(4) VALUE 0.
         05 CPT-ANOMALIE           PIC 9(4) VALUE 0.
       
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
       10000-INIT-PGM.
      *---------------* 
           OPEN INPUT F-STOCK
           OPEN OUTPUT F-COMPTA
           OPEN OUTPUT F-DEPASSEMENT
           OPEN OUTPUT F-STATS
           .
       20000-TRAITEMENT.
      *-----------------* 
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
       21000-COMPTA.
      *-------------*
           MOVE NUM-STOCK TO NUM-STOCK-F
           COMPUTE PRIX-TOT-F = NB-PRODUIT * PU-PRODUIT
           MOVE FORMAT-COMPTA TO COMPTA
           WRITE COMPTA
           ADD 1 TO CPT-COMPTA
           .
       22000-ALERTE.
      *-------------*
           IF (NB-PRODUIT < NB-MIN)
             DISPLAY "Réapprovisionnement du produit N° " NUM-STOCK
             ADD 1 TO CPT-ANOMALIE
           .
       23000-DEPASSEMENT.
      *------------------*
           IF (NB-PRODUIT >= NB-MIN AND PU-PRODUIT > 150)
             MOVE NUM-STOCK TO NUM-STOCK-F
             COMPUTE PRIX-TOT-F = NB-PRODUIT * PU-PRODUIT
             MOVE FORMAT-COMPTA TO depassement
             WRITE DEPASSEMENT
             ADD 1 TO CPT-DEPASSEMENT
           .
       30000-END-PGM.
      *--------------*
           PERFORM 31000-STATS
           CLOSE F-STOCK
           CLOSE f-compta
           CLOSE f-depassement
           close f-stats
           .
       31000-STATS.
      *------------*
      * Write Stock
           MOVE 'STOCK' TO FICHIER-N
           MOVE CPT-STOCK TO CPT-F
           PERFORM 31100-WRITE-STATS
      * Write Compta
           MOVE 'COMPTA' TO FICHIER-N
           MOVE CPT-COMPTA TO CPT-F
           PERFORM 31100-WRITE-STATS
      * Write Depassement
           MOVE 'DEPASSEMENT' TO FICHIER-N
           MOVE CPT-DEPASSEMENT TO CPT-F
           PERFORM 31100-WRITE-STATS
      * Write Anomalie
           MOVE 'ANOMALIE' TO FICHIER-N
           MOVE CPT-ANOMALIE TO CPT-F
           PERFORM 31100-WRITE-STATS
           .
       31100-WRITE-STATS.
      *------------------*
           MOVE FORMAT-STATS TO STATS
           WRITE STATS
           .