       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB10.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT f-client1 ASSIGN dclient1.
           SELECT f-client2 ASSIGN dclient2.
           SELECT f-clientf ASSIGN dclientf.
           SELECT f-stats assign ddstatsf.
      *********************************
      *    D A T A   D I V I S I O N
      *********************************
       DATA DIVISION.
       FILE SECTION.
       fd f-client1
           BLOCK CONTAINS 0
           DATA RECORD IS e-client1.
       01 E-CLIENT1.
         05 CLIENT1-ID             PIC 9(6). 
         05                        PIC X(44).  
       fd f-client2
           BLOCK CONTAINS 0
           DATA RECORD IS e-client2.
       01 E-CLIENT2.
         05 CLIENT2-ID             PIC 9(6). 
         05                        PIC X(44). 
       fd f-clientf.
       01 CLIENTF                  PIC X(80).
       fd f-stats.
       01 STATS                    PIC X(80).

       WORKING-STORAGE SECTION.
      * --------------- Compteurs ----------- 
       01 CPT.
         05 CPT-CLIENT1            PIC 9(4) VALUE 0.
         05 CPT-CLIENT2            PIC 9(4) VALUE 0.
         05 CPT-CLIENTF            PIC 9(4) VALUE 0.
      * ---------------- Format -------------- 
       01 FORMAT-STATS.
         05                        PIC X(43) VALUE 
           'Nombre d’enregistrement lus dans '.
         05 FICHIER-N              PIC X(10).
         05                        PIC X(3) VALUE ' : '.
         05 CPT-F                  PIC Z(3)9.
       
      * Stock le client écrit en dernier
       77 CLIENT-ID-TEMP           PIC 9(6).

      * Variables de fin de fichier 
       77 EOF-TRUE                 PIC X VALUE "Y".
       77 EOF-CLIENT1              PIC X VALUE "F".
       77 EOF-CLIENT2              PIC X VALUE "F".

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           PERFORM 10000-INIT-PGM
           PERFORM 20000-TRAITEMENT
           PERFORM 30000-END-PGM
           .
      * Ouvre les fichiers et effectue la première lecture
       10000-INIT-PGM. 
           OPEN INPUT F-CLIENT1
           OPEN INPUT F-CLIENT2
           OPEN OUTPUT F-CLIENTF
           PERFORM READ-CLIENT1
           PERFORM READ-CLIENT2
           .
      * Parcours les fichiers
       20000-TRAITEMENT. 
           PERFORM UNTIL
            EOF-CLIENT1 = EOF-TRUE AND EOF-CLIENT2 = EOF-TRUE
             EVALUATE TRUE
      *        On ajoute les client1 restant
               WHEN
                EOF-CLIENT1 NOT = EOF-TRUE AND EOF-CLIENT2 = EOF-TRUE
                 PERFORM UNTIL EOF-CLIENT1 = EOF-TRUE
                   PERFORM 21000-HANDLE-CLIENT1
                 END-PERFORM
      *        On ajoute les client2 restant
               WHEN
                EOF-CLIENT1 = EOF-TRUE AND EOF-CLIENT2 NOT = EOF-TRUE
                 PERFORM UNTIL EOF-CLIENT2 = EOF-TRUE
                   PERFORM 22000-HANDLE-CLIENT2
                 END-PERFORM
      *        On ajoute le client le plus petit 
               WHEN OTHER
                 PERFORM 23000-COMPARE-CLIENT
             end-evaluate
           end-perform
           .
      * Lit le fichier client1
       READ-CLIENT1.
           READ F-CLIENT1
             AT END
               MOVE EOF-TRUE TO EOF-CLIENT1
           END-READ
           .
      * Lit le fichier client2
       READ-CLIENT2.
           READ F-CLIENT2
             AT END
               MOVE EOF-TRUE TO EOF-CLIENT2
           END-READ
           .
      * Gere l'ecriture de client 1 dans client f 
      * Ainsi que la lecture de client 1    
       21000-HANDLE-CLIENT1.
             MOVE E-CLIENT1 TO CLIENTF
             WRITE CLIENTF
             ADD 1 TO CPT-CLIENT1
             MOVE CLIENT1-ID TO CLIENT-ID-TEMP
             PERFORM READ-CLIENT1               
           .
      * Regarde si le dernier client ajoute est identique a client 1    
       21100-CHECK-CLIENT1-DOUBLON.
           IF (CLIENT1-ID = CLIENT-ID-TEMP)
             DISPLAY "DOUBLON CLIENT 1 : " CLIENT-ID-TEMP 
             PERFORM 30000-END-PGM      
           END-IF
           .
      * Gere l'ecriture de client 2 dans client f 
      * Ainsi que la lecture de client 2     
       22000-HANDLE-CLIENT2.
               MOVE E-CLIENT2 TO CLIENTF
               WRITE CLIENTF
               ADD 1 TO CPT-CLIENT2
               MOVE CLIENT2-ID TO CLIENT-ID-TEMP
               PERFORM READ-CLIENT2        
           .
      * Regarde si le dernier client ajoute est identique a client 2    
       22100-CHECK-CLIENT2-DOUBLON.
           IF (CLIENT2-ID = CLIENT-ID-TEMP)
             DISPLAY "DOUBLON CLIENT 2 : " CLIENT-ID-TEMP 
             PERFORM 30000-END-PGM      
           END-IF
           .
      * Verifie quel client est le plus petit
       23000-COMPARE-CLIENT.
           PERFORM 23100-CHECK-CLIENT
           IF (CLIENT1-ID < CLIENT2-ID)
             PERFORM 21000-HANDLE-CLIENT1
           ELSE
             PERFORM 22000-HANDLE-CLIENT2
           end-if
           .
      * Verifie si il n y a pas de problème de doublons
       23100-CHECK-CLIENT.
           IF (CLIENT1-ID = CLIENT2-ID)
             DISPLAY "IDENTIQUE : " CLIENT1-ID
             PERFORM 30000-END-PGM     
           END-IF
           PERFORM 21100-CHECK-CLIENT1-DOUBLON
           PERFORM 22100-CHECK-CLIENT2-DOUBLON
           .
      * Affiche les stats et ferme les fichiers
       30000-END-PGM.
           PERFORM 31000-STATS
           CLOSE F-CLIENT1
           CLOSE F-CLIENT2
           CLOSE F-CLIENTF
           STOP RUN
           .
      * Gere le fichier stats
       31000-STATS.
           OPEN OUTPUT F-STATS
      *    Client1
           MOVE 'CLIENT1' TO FICHIER-N
           MOVE CPT-CLIENT1 TO CPT-F
           PERFORM 31100-WRITE-STATS
      *    Client2
           MOVE 'CLIENT2' TO FICHIER-N
           MOVE CPT-CLIENT2 TO CPT-F
           PERFORM 31100-WRITE-STATS
      *    Client3
           COMPUTE CPT-CLIENTF = CPT-CLIENT1 + CPT-CLIENT2
           MOVE 'CLIENT-F' TO FICHIER-N
           MOVE CPT-CLIENTF TO CPT-F
           PERFORM 31100-WRITE-STATS
           CLOSE F-STATS
           .
      * Ecrit dans le fichier stats
       31100-WRITE-STATS.
           MOVE FORMAT-STATS TO STATS
           WRITE STATS
           .