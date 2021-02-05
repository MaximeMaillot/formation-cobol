       ID DIVISION.
       PROGRAM-ID. TSUB.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       LINKAGE SECTION.
       01 variableByContent PIC 99.
       01 variableByReference pic 99.
       01 computeRes pic 99.


       PROCEDURE DIVISION USING variableByContent variableByReference
           RETURNING computeRes.
           
           Compute computeRes = 
            variableByContent + variableByReference
           ADD 1 TO variableByContent
           ADD 1 TO variableByReference
           GOBACK
           .