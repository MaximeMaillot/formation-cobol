       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB12.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT f-pop ASSIGN dpop.
           SELECT f-stats ASSIGN dstatsp.
      *********************************
      *    D A T A   D I V I S I O N
      *********************************
       DATA DIVISION.
       FILE SECTION.
       fd f-pop
           BLOCK CONTAINS 0
           DATA RECORD IS e-pop.
       01 e-pop.
         05 DEPARTEMENT            PIC 9(2).
         05 CANTON.
           10                      PIC X(2).
           10 CT-NUM               PIC 9.
         05 COMMUNE.
           10                      PIC X.
           10 C-NUM                PIC 9.
         05 NB-HABITANT            PIC 9(5).
         05                        PIC X(28).
       fd f-stats.
       01 stats                    PIC X(80).

       WORKING-STORAGE SECTION.
      * --------------- Compteurs ----------- 

      * ---------------- Format --------------    
       01 FORMAT-TITLE.
         05                        PIC X(20).
         05                        PIC X(60)
           VALUE 'STATISTIQUE DE LA POPULATION'.
       01 FORMAT-DEPARTEMENT.
         05                        PIC X(12) VALUE 'Departement '.
         05 DEPARTEMENT            PIC 99.
         05                        PIC X(3) VALUE ' : '.
         05                        PIC X(63).
       01 FORMAT-CANTON.
         05                        PIC X(4).
         05                        PIC X(7) VALUE 'Canton '.
         05 CANTON                 PIC X(3).
         05                        PIC X(3) VALUE ' : '.
         05                        PIC X(63).
       01 FORMAT-COMMUNE.
         05                        PIC X(10).
         05                        PIC X(9) VALUE 'commune '.
         05 COMMUNE                PIC X(2).
         05                        PIC X(17).
         05 NB-HABITANT            PIC Z(4)9.
         05                        PIC X(37).
       01 FORMAT-TOTAL-CANTON.
         05                        PIC X(4).
         05                        PIC X(13) VALUE 'Total canton '.
         05 CANTON                 PIC X(3).
         05                        PIC X(17).
         05 TOTAL-CANTON           PIC Z(5)9.
         05                        PIC X(37).
       01 FORMAT-TOTAL-DEPARTEMENT.
         05                        PIC X.
         05                        PIC X(18) VALUE 'Total departement '.
         05 DEPARTEMENT            PIC 99.
         05                        PIC X(16).
         05 TOTAL-DEPARTEMENT      PIC Z(5)9.
         05                        PIC X(37).
       01 FORMAT-TOTAL-GENERAL.
         05                        PIC X(4).
         05                        PIC X(13) VALUE 'Total general'.
         05                        PIC X(20).
         05 TOTAL-GENERAL          PIC Z(5)9.
         05                        PIC X(37).
          

      * ---------- Variable de calcul -------------
       01 TOTAUX.
         05 TOTAL-CANTON           PIC 9(6).
         05 TOTAL-DEPARTEMENT      PIC 9(6).
         05 TOTAL-GENERAL          PIC 9(6).

       77 TEMP-CANTON               PIC X(3).
       77 TEMP-DEPARTEMENT          PIC 99.  

      * Variables de fin de fichier 
       77 EOF-TRUE                 PIC X VALUE "Y".
       77 EOF-POP                  PIC X VALUE "F".

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           PERFORM 10000-INIT-PGM
           PERFORM 20000-TRAITEMENT
           PERFORM 30000-END-PGM
           .
       10000-INIT-PGM.
           open input f-pop
           open output f-stats
           perform READ-POP
           perform POP-FIRST-READ
           .
       20000-TRAITEMENT.
           PERFORM UNTIL EOF-POP = EOF-TRUE
             IF (TEMP-DEPARTEMENT = DEPARTEMENT of e-pop)
               IF (TEMP-CANTON = CANTON of e-pop)
                 perform POP-NOCHANGE
               ELSE
                 perform POP-CHANGE-CANTON
               END-IF
             ELSE
               perform POP-CHANGE-DEPT
             END-IF
           perform READ-POP
           END-PERFORM
           .
       30000-END-PGM.
           perform POP-LAST-READ
           CLOSE f-pop
           close f-stats
           stop run
           .
       READ-POP.
           READ f-pop
             AT END
               MOVE EOF-TRUE TO EOF-POP
           END-READ
           .
       POP-FIRST-READ.
           perform WRITE-TITLE
           perform WRITE-DEPARTEMENT
           perform WRITE-CANTON
           MOVE CANTON of e-pop TO TEMP-CANTON
           MOVE DEPARTEMENT of e-pop TO TEMP-DEPARTEMENT
           .
       POP-NOCHANGE.
           ADD NB-HABITANT of e-pop TO TOTAL-CANTON of TOTAUX
           perform WRITE-COMMUNE
           .
       POP-CHANGE-CANTON.
           ADD TOTAL-CANTON of TOTAUX TO TOTAL-DEPARTEMENT of TOTAUX
           perform WRITE-TOT-CANTON
           perform WRITE-CANTON
           perform WRITE-COMMUNE
           MOVE CANTON of e-pop TO TEMP-CANTON
           initialize TOTAL-CANTON of TOTAUX
           .
       POP-CHANGE-DEPT.
           ADD TOTAL-DEPARTEMENT of TOTAUX TO TOTAL-GENERAL of TOTAUX
           perform WRITE-TOT-CANTON
           perform WRITE-TOT-DEPT
           perform WRITE-DEPARTEMENT
           perform WRITE-CANTON
           perform WRITE-COMMUNE
           MOVE CANTON of e-pop TO TEMP-CANTON
           MOVE DEPARTEMENT of e-pop TO TEMP-DEPARTEMENT
           initialize TOTAL-CANTON of TOTAUX
           initialize TOTAL-DEPARTEMENT of TOTAUX
           .
       POP-LAST-READ.
           ADD TOTAL-CANTON of TOTAUX TO TOTAL-DEPARTEMENT of TOTAUX
           ADD TOTAL-DEPARTEMENT of TOTAUX TO TOTAL-GENERAL of TOTAUX
           perform WRITE-TOT-CANTON
           perform WRITE-TOT-DEPT
           perform WRITE-TOT-GEN
           .
       WRITE-TITLE.
           MOVE FORMAT-TITLE TO stats
           WRITE stats
           .
       WRITE-DEPARTEMENT.
           MOVE DEPARTEMENT of e-pop 
            TO DEPARTEMENT of FORMAT-DEPARTEMENT
           MOVE FORMAT-DEPARTEMENT TO stats
           WRITE stats
           .
       WRITE-CANTON.
           MOVE CANTON of e-pop TO CANTON of FORMAT-CANTON
           MOVE FORMAT-CANTON TO stats
           WRITE stats
           .
       WRITE-COMMUNE.
           MOVE COMMUNE of e-pop TO COMMUNE of FORMAT-COMMUNE
           MOVE NB-HABITANT of e-pop TO NB-HABITANT of FORMAT-COMMUNE
           MOVE FORMAT-COMMUNE TO stats
           WRITE stats
           .
       WRITE-TOT-CANTON.
           MOVE TOTAL-CANTON of TOTAUX 
            TO TOTAL-CANTON of FORMAT-TOTAL-CANTON
           MOVE FORMAT-TOTAL-CANTON TO stats
           WRITE stats
           .
       WRITE-TOT-DEPT.
           MOVE TOTAL-DEPARTEMENT of TOTAUX 
            TO TOTAL-DEPARTEMENT of FORMAT-TOTAL-DEPARTEMENT
           MOVE FORMAT-TOTAL-DEPARTEMENT TO stats
           WRITE stats
           .
       WRITE-TOT-GEN.
           MOVE TOTAL-GENERAL of TOTAUX 
            TO TOTAL-GENERAL of FORMAT-TOTAL-GENERAL
           MOVE FORMAT-TOTAL-GENERAL TO stats
           WRITE stats
           .