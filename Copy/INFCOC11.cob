       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBCO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *********************************
      *    D A T A   D I V I S I O N
      *********************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 I pic 999.

       COPY TABMESS.
       01 tab redefines table-message.
         02 mess occurs 150 pic x(60).

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 150
              DISPLAY mess(I)
           END-PERFORM 
           stop run
           .
 