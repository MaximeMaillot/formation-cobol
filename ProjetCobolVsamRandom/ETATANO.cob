       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJERR.
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
       fd f-error is external
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
       01 param.
         02 L-ERROR-CODE             PIC 9(3).
         02 err-label                PIC x(60).
         02 flag                     PIC 9.
           88 flag-open value 0.
           88 flag-continue value 5.
           88 flag-close value 9.
         02 CR-FILE                  PIC 99.

       PROCEDURE DIVISION USING param.
           EVALUATE true 
             WHEN flag-continue 
              MOVE L-ERROR-CODE TO error-key-9
              READ f-error
              MOVE err-message TO err-label
              MOVE CR-ERRVS TO CR-FILE
             WHEN flag-open
               OPEN INPUT f-error
             WHEN flag-close 
              close f-error
           END-EVALUATE
           GOBACK
           .