       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB13.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT f-client ASSIGN dfalgo.
           SELECT f-addr ASSIGN daddres.
      *********************************
      *    D A T A   D I V I S I O N
      *********************************
       DATA DIVISION.
       FILE SECTION.
       fd f-client
           BLOCK CONTAINS 0
           DATA RECORD IS e-client.
       01 e-client.
         05 TYPE-ENR PIC 9.
         05 NUM-COMPTE PIC 9(6).
         05 CORPS-ENR PIC X(73).
         05 CORPS-ENR-1 redefines CORPS-ENR.
           10 NOM-CLIENT PIC X(20).
           10 PIC X(53).
         05 CORPS-ENR-2 redefines CORPS-ENR.
           10 ADRESSE PIC X(25).
           10 DEPARTEMENT PIC X(10).
           10 PIC X(38).
         05 CORPS-ENR-3 redefines CORPS-ENR.
           10 MONTANT PIC 9(4).
           10 PIC X(69).
       fd f-addr.
       01 ADRESSES PIC X(80).

       WORKING-STORAGE SECTION.

       01 ADRESSES-FORMAT.
         05 NUM-ADRESSE PIC 9(3) VALUE 100.
         05 NUM-COMPTE-A PIC 9(6).
         05 NOM-CLIENT-A PIC X(20).
         05 ADRESSE-A PIC X(25).
         05 DEPARTEMENT-A PIC X(10).
         05 TOTAL-FACTURE-A PIC 9(5).
         05 PIC X(11).

       77 prev-type PIC 9.
       77 temp-num-compte PIC 9(6).

      * Variables de fin de fichier 
       77 EOF-TRUE                 PIC X VALUE "Y".
       77 EOF-CLIENT               PIC X VALUE "F".

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           PERFORM 10000-INIT-PGM
           PERFORM 20000-TRAITEMENT
           PERFORM 30000-END-PGM
           .
       10000-INIT-PGM.
           open input f-client
           open output f-addr
           perform READ-CLIENT
           MOVE NUM-COMPTE to temp-num-compte
           .
       20000-TRAITEMENT.
           PERFORM UNTIL EOF-CLIENT = EOF-TRUE
             EVALUATE TYPE-ENR
               WHEN 1
                 IF prev-type NOT = 0
                   perform 29000-SKIP-CLIENT
                 ELSE
                   perform 21000-FORMAT-ADDR-1
                   MOVE 1 to prev-type
                 END-IF
               WHEN 2
                 IF prev-type NOT = 1
                   perform 29000-SKIP-CLIENT
                 else
                   perform 22000-FORMAT-ADDR-2
                   MOVE 2 to prev-type
                 end-if
               WHEN 3
                 IF prev-type NOT = 2
                   perform 29000-SKIP-CLIENT 
                 else
                   perform 23000-FORMAT-ADDR-3           
                 end-if
               WHEN OTHER
                 perform 29000-SKIP-CLIENT
             end-evaluate
             IF EOF-CLIENT NOT = EOF-TRUE
               perform READ-CLIENT
             END-IF
             IF (NUM-COMPTE NOT = temp-num-compte AND prev-type = 2)
               perform WRITE-ADDR
             END-IF
             MOVE NUM-COMPTE to temp-num-compte
           END-PERFORM
           .
       30000-END-PGM.
           CLOSE f-client
           close f-addr
           stop run
           .
       READ-CLIENT.
           READ f-client
             AT END
               MOVE EOF-TRUE TO EOF-client
           END-READ
           .
       WRITE-ADDR.
           MOVE ADRESSES-FORMAT TO ADRESSES
           WRITE ADRESSES
           INITIALIZE TOTAL-FACTURE-A
           INITIALIZE prev-type
           .
       21000-FORMAT-ADDR-1.
           MOVE NUM-COMPTE TO NUM-COMPTE-A
           MOVE NOM-CLIENT TO NOM-CLIENT-A
           .
       22000-FORMAT-ADDR-2.
           MOVE ADRESSE TO ADRESSE-A
           MOVE DEPARTEMENT TO DEPARTEMENT-A
           .
       23000-FORMAT-ADDR-3.
           ADD MONTANT TO TOTAL-FACTURE-A
           .
       29000-SKIP-CLIENT.
           perform until num-compte NOT = temp-num-compte or
            EOF-CLIENT = EOF-TRUE
            perform READ-CLIENT
           end-perform
           INITIALIZE TOTAL-FACTURE-A
           INITIALIZE prev-type
           .