       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBDATE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01 DATE-ENTERED.
         05 YEAR-E                 PIC 9(4).
         05 MONTH-E                PIC 9(2).
         05 DAY-E                  PIC 9(2).
         05 EMPTY                  PIC X(4).
           
       01 MONTH-V.
         10                        PIC 99 VALUE 31.
         10                        PIC 99 VALUE 29.
         10                        PIC 99 VALUE 31.
         10                        PIC 99 VALUE 30.
         10                        PIC 99 VALUE 31.
         10                        PIC 99 VALUE 30.
         10                        PIC 99 VALUE 31.
         10                        PIC 99 VALUE 31.
         10                        PIC 99 VALUE 30.
         10                        PIC 99 VALUE 31.
         10                        PIC 99 VALUE 30.
         10                        PIC 99 VALUE 31.
       01 TAB-MONTH redefines MONTH-V.
         10 NB-JOURS               PIC 99 occurs 12.
           
       01 MODULO-N.
         05 MODULO4                PIC 99.
         05 MODULO100              PIC 99.
         05 MODULO400              PIC 99.

       77 LIGNE PIC X(80) VALUE ALL '-'.
               
       PROCEDURE DIVISION.
           PERFORM 10000-INIT-PGM
           PERFORM 20000-TRAITEMENT
           PERFORM 30000-END-PGM
           .

       10000-INIT-PGM.
           ACCEPT DATE-ENTERED
           .
       20000-TRAITEMENT.
           PERFORM UNTIL DATE-ENTERED = '00000000'
             perform 21000-VALIDATE-DATE
             INITIALIZE DATE-ENTERED
             ACCEPT DATE-ENTERED
             DISPLAY LIGNE
           END-PERFORM
           .
       30000-END-PGM.
           DISPLAY "PROGRAMME ARRETE PAR DEMANDER UTILISATEUR"
           stop run
           .
       21000-VALIDATE-DATE.
           DISPLAY "YEAR : " YEAR-E ", MONTH : " MONTH-E, "DAY : " DAY-E
           IF DATE-ENTERED IS NOT NUMERIC OR EMPTY NOT = SPACE
             perform 21100-DATE-ERROR
           ELSE
             EVALUATE TRUE
      *        Check mois       
             WHEN (MONTH-E > 12 OR MONTH-E < 1)
               perform 21100-DATE-ERROR
      *        Check jour     
             WHEN (DAY-E > NB-JOURS(MONTH-E) OR DAY-E < 1)
               perform 21100-DATE-ERROR
      *        Check bisextile       
             WHEN (MONTH-E = 2 AND DAY-E = 29)
               perform 21200-GET-MODULOS
               IF (NOT (MODULO4 = 0 AND MODULO100 NOT = 0))
                OR MODULO400 = 0
                 perform 21100-DATE-ERROR
               ELSE
                 perform 21300-DATE-VALIDE
               END-IF
             WHEN OTHER
                 perform 21300-DATE-VALIDE
             END-EVALUATE
           END-IF
           .
       21100-DATE-ERROR.
             DISPLAY "ERREUR"
           .
       21200-GET-MODULOS.
           DIVIDE YEAR-E BY 4 GIVING MODULO4 REMAINDER MODULO4
           DIVIDE YEAR-E BY 100 GIVING MODULO100 REMAINDER MODULO100
           DIVIDE YEAR-E BY 400 GIVING MODULO400 REMAINDER MODULO400
           .
       21300-DATE-VALIDE.
               DISPLAY "VALIDE"
           .