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
           SELECT f-etatano ASSIGN ETATANO
            FILE STATUS IS CR-ETATANO.

      *********************************
      *    D A T A   D I V I S I O N
      *********************************
       DATA DIVISION.
       FILE SECTION.
       fd f-error is external
           DATA RECORD IS e-error.
       01 e-error.
           02 error-key-9          pic 9(3).
           02 error-key-x 
            REDEFINES error-key-9  PIC x(3).
           02 err-message          pic x(60).
           02                      PIC X(17).

       fd f-etatano is external.
       01 etatano.
           COPY CANO.


       WORKING-STORAGE SECTION.
       
       01 CR-ERRVS                 PIC 99.
       01 CR-ETATANO               PIC 99.

       LINKAGE SECTION.
       01 L-CODE-MVT               PIC X.
       01 L-MATRICULE              PIC X(6).
       01 L-ERROR-CODE             PIC 9(3).

       PROCEDURE DIVISION USING L-CODE-MVT L-MATRICULE L-ERROR-CODE.
           PERFORM 10000-INIT-PGM
           PERFORM 20000-TRAITEMENT
           PERFORM 30000-END-PGM
           .

       10000-INIT-PGM.
           continue
           .

       20000-TRAITEMENT.
           MOVE L-ERROR-CODE TO error-key-9
           READ f-error
           MOVE L-MATRICULE to NUM-MAT
           MOVE L-CODE-MVT TO CODE-MVT-ANO
           MOVE err-message TO LIB-MESS
           WRITE etatano
           .
           
       30000-END-PGM.
           GOBACK
           .     