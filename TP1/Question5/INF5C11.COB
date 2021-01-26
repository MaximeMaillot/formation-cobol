       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB5.
      ****************************************************************
      *    D A T A   D I V I S I O N
      ***********************************************************
       DATA DIVISION.
      ****************************************************************
      *    W O R K I N G - S T O R A G E   S E C T I O N
      ****************************************************************
       WORKING-STORAGE SECTION.
      *--------------DEFINITION DES VARIABLES---------------------
       01  MOUVEMENT.
           05 MATRICULE        PIC 9(6).
           05 CODE-MOUVEMENT   PIC 9(1).
           05 CODE-N           PIC X(40).
       01 CODE-1.
           05 SIGNE            PIC X(1).
           05 TAUX             PIC 9(2).
       01 CODE-2.
           05 NOM-PRENOM       PIC X(20).
       01 CODE-3.
           05 ADRESSE          PIC X(18).
           05 CODE-POSTAL      PIC 9(5).
           05 VILLE            PIC X(12).
       01 CODE-4.
           05 TYPE-P           PIC X(1).
           05 PRIME            PIC 9(4)V9(2).

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           ACCEPT MOUVEMENT
      * BEFORE => EQUIVALENT DO WHILE
           PERFORM MOUVEMENT-L
            WITH TEST BEFORE UNTIL MATRICULE = 200046.
           STOP RUN.
      * PARCOURS MOUVEMENT
       MOUVEMENT-L.
           DISPLAY "MOUVEMENT : " MOUVEMENT
           IF MATRICULE IS NOT NUMERIC
              DISPLAY "1 - Matricule non numérique"
           ELSE
            EVALUATE CODE-MOUVEMENT
             WHEN 1
               MOVE CODE-N TO CODE-1
               IF SIGNE NOT EQUAL '-' AND SIGNE NOT EQUAL '+'
                 DISPLAY "3 - Signe différent de + ou -"
               END-IF
               IF TAUX IS NOT NUMERIC
                 DISPLAY "4 - Taux non numérique"
               END-IF
             WHEN 2
               MOVE CODE-N TO CODE-2
               IF NOM-PRENOM EQUAL SPACE
                 DISPLAY "5 - Le nom n'est pas renseigné"
               END-IF
             WHEN 3
               MOVE CODE-N TO CODE-3
               IF ADRESSE EQUAL SPACE
                AND CODE-POSTAL EQUAL SPACE
                AND VILLE EQUAL SPACE
                 DISPLAY "6 - Au moins une zone doit être saisie"
               ELSE
                   IF CODE-POSTAL IS NOT NUMERIC
                    AND CODE-POSTAL NOT EQUAL SPACE
                     DISPLAY "7 - Code postal invalide"
                   END-IF
               END-IF
             WHEN 4
               MOVE CODE-N TO CODE-4
               IF (TYPE-P < 0) OR (TYPE-P > 9)
                 DISPLAY "8 - Type de prime invalide"
               END-IF
               IF PRIME IS NOT NUMERIC
                 DISPLAY "9 - Prime non numérique"
               END-IF
             WHEN OTHER
               DISPLAY "2 - Code mouvement inconnu"
            END-EVALUATE
           END-IF
           ACCEPT MOUVEMENT.
