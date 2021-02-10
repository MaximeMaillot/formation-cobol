       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJANO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT f-error ASSIGN ERRVS
            ORGANIZATION IS INDEXED
            ACCESS MODE IS RANDOM
            RECORD KEY IS error-key-x
            FILE STATUS IS CR-ERRVS.

      *********************************
      *    D A T A   D I V I S I O N
      *********************************
       DATA DIVISION.
       FILE SECTION.
       fd f-error
           DATA RECORD IS e-error.
       01 e-error.
           02 error-key-9          pic 9(3).
           02 error-key-x 
            REDEFINES error-key-9  PIC x(3).
           02 err-message          pic x(60).
           02                      PIC X(17).


       WORKING-STORAGE SECTION.
       
       01 CR-ERRVS                 PIC 99.

       LINKAGE SECTION.
       01 L-ERROR-CODE             PIC 9(3).
       01 flag                     PIC 9.
       01 err-label                PIC x(60).

       PROCEDURE DIVISION 
           USING L-ERROR-CODE FLAG err-label.
           EVALUATE FLAG
            WHEN 1
              PERFORM 10000-INIT-PGM
            WHEN 5
              PERFORM 20000-TRAITEMENT
            WHEN 9
              PERFORM 30000-END-PGM
            WHEN OTHER
              GOBACK
           END-EVALUATE
           GOBACK
           .

       10000-INIT-PGM.
           OPEN INPUT f-error
           .

       20000-TRAITEMENT.
           MOVE L-ERROR-CODE TO error-key-9
           READ f-error
           MOVE err-message TO err-label
           .
           
       30000-END-PGM.
           CLOSE f-error
           .     