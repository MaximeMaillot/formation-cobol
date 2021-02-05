       ID DIVISION.
       PROGRAM-ID. TPGM.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 variableByContent PIC 99 VALUE 1.
       01 variableByReference pic 99 VALUE 2.
       01 computeRes pic 99.

       01 test-appel-by-ident PIC x(8) value 'TESTSUB'.

       PROCEDURE DIVISION.
           DISPLAY "BEFORE CALL => "
           DISPLAY variableByContent ", " variableByReference
           DISPLAY computeRes
           CALL 'TESTSUB' USING
            BY CONTENT variableByContent
            BY REFERENCE variableByReference
           RETURNING computeRes

           DISPLAY "AFTER CALL => "
           DISPLAY variableByContent ", " variableByReference
           DISPLAY computeRes
      *    call by ident
           DISPLAY "BEFORE CALL => "
           DISPLAY variableByContent ", " variableByReference
           DISPLAY computeRes
           CALL test-appel-by-ident USING
            BY CONTENT variableByContent
            BY REFERENCE variableByReference
           RETURNING computeRes

           DISPLAY "AFTER CALL => "
           DISPLAY variableByContent ", " variableByReference
           DISPLAY computeRes
           stop run
           .
