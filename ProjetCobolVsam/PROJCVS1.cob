       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJETM.
      ***********************************************
      *    E N V I R O N M E N T   D I V I S I O N
      ***********************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT f-assures-in ASSIGN ASSUR3
            ORGANIZATION IS INDEXED
            ACCESS MODE SEQUENTIAL
            RECORD KEY IS MAT-X3
            FILE STATUS IS CR-ASSURES-IN.

           SELECT f-mvt ASSIGN AS-FMVT
            ORGANIZATION SEQUENTIAL
            FILE STATUS IS CR-MVT.

           SELECT f-assures-out ASSIGN ASSUR4
            ORGANIZATION IS INDEXED
            ACCESS MODE SEQUENTIAL 
            RECORD KEY IS MAT-X4 
            FILE STATUS IS CR-ASSURES-OUT.
           
           SELECT f-etatano ASSIGN ETATANO
            FILE STATUS IS CR-ETATANO.

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
       fd f-assures-in
           DATA RECORD IS e-assures-in.
       01 e-assures-in.
           COPY CASSURE3.


       fd f-mvt
           DATA RECORD IS e-mvt.
       01 e-mvt.
           COPY CMVTPROJ.

       
       fd f-assures-out.
       01 assures-out.
           COPY CASSURE4.

       fd f-etatano.
       01 etatano.
           COPY CANO.

       fd f-error is external
           DATA RECORD IS e-error.
       01 e-error.
           02 error-key-9          pic 9(3).
           02 error-key-x 
            REDEFINES error-key-9  PIC x(3).
           02 err-message          pic x(60).
           02                      PIC X(17).
       

       WORKING-STORAGE SECTION.
       
       01 CR-ASSURES-IN                  PIC 99.
           88 EOF-ASSURES-IN VALUE 10.
       01 CR-MVT                         PIC 99.
           88 EOF-MVT VALUE 10.
       01 CR-ASSURES-OUT                 PIC 99.
       01 CR-ETATANO                     PIC 99.
       01 CR-ERRVS                       PIC 99.

       01 CPT.
         02 CPT-FILLER.
           05                            PIC 99 OCCURS 4.
           
         02 CPT-READ.
           05 CPT-ASSURES-IN             PIC 99.
           05 CPT-MVT                    PIC 99.
           05 CPT-CREATE                 PIC 99.
           05 CPT-ASSURES-OUT            PIC 99.
           05 CPT-REWRITE                PIC 99.
           05 CPT-DELETE                 PIC 99.

         02 CPT-ANOMALIE                 PIC 99.

         02 CPT-ANO.
           05 CPT-ANO-MVT                PIC 99.
           05 CPT-ANO-CREATE             PIC 99.
           05 CPT-ANO-REWRITE            PIC 99.
           05 CPT-ANO-DELETE             PIC 99.
         02 TAB-CPT-ANO REDEFINES CPT-ANO.
           05 CPT-ANO-T                  PIC 99 occurs 4.
           
       01 TAB-CPT REDEFINES CPT.
        02 CPT-STATS                     PIC 99 occurs 15.

       77 ano-pgm                        PIC X(7) VALUE 'ETATANO'.

       77 I                              PIC 999.

       77 ERR-CODE                       PIC 999.

       01 weekday-desc.
         COPY weekday.
       01 weekday-tab.
         05 weekday PIC X(10) occurs 7.

       01 DATE-F.
         05 YEAR-F PIC 9999.
         05 MONTH-F PIC 99.
         05 DAY-F PIC 99.
       01 TIME-F.
         05 HOUR-F PIC 99.
         05 MINUTE-F PIC 99.
         05 SECOND-F PIC 99.
       01 WEEKDAY-F PIC 9.

      * -------------------- FORMAT ANO ----------------
       01 FORMAT-HEADER-TITLE-ANO.
         05 PIC X(31).
         05 PIC X(18) value 'ETAT DES ANOMALIES'.
         05 PIC X(31).
       
       01 FORMAT-MAJ-ANO.
         05 PIC X(14) value 'MISE A JOUR DU'.
         05 PIC X(3) value ' : '.
         05 PIC X(3).
         05 DAY-F PIC 9(2).
         05 PIC X value '/'.
         05 MONTH-F PIC 9(2).
         05 PIC X value '/'.
         05 YEAR-F PIC 9(4).
         05 PIC X(50).
       01 FORMAT-GROUPE-ANO.
         05 PIC X(14) value 'GROUPE'.
         05 PIC X(3) value ' : '.
         05 PIC X(3).
         05 PIC X(10) value 'API11'.
         05 PIC X(50).

       01 FORMAT-EMPTY-LIGNE-ANO.
         05 PIC X(80) value space.
         
       01 FORMAT-LIGNE-TAB-ANO.
         05 PIC X(80) value all '-'.

      * ------------------------------------------------ 

      * -------------------- FORMAT STATS --------------

       01 FORMAT-HEADER-STATS-1.
         05 PIC X.
         05 PIC X(7) value 'User : '.
         05 PIC X(10) value 'API11'.
         05 PIC X(28).
         05 WEEKDAY-S PIC X(10).
         05 PIC X(4) value ' Le '.
         05 DAY-F PIC 9(2).
         05 PIC X value '/'.
         05 MONTH-F PIC 9(2).
         05 PIC X value '/'.
         05 YEAR-F PIC 9(4).
       
       01 FORMAT-HEADER-STATS-2.
         05 PIC X(65).
         05 HOUR-F PIC 9(2).
         05 PIC X value ':'.
         05 MINUTE-F PIC 9(2).
      
       01 FORMAT-TITLE-STATS.
         05 PIC X value '|'.
         05 PIC X(24).
         05 PIC X(12) value 'STATISTIQUES'.
         05 PIC X(34).
         05 PIC X value '|'.

       01 FORMAT-ENRGMT-STATS.
         05 PIC X value 'I'.
         05 PIC X.
         05 message-erreur PIC X(60).
         05 PIC X(6).
         05 CPTI-F PIC 99.
         05 PIC X.
         05 PIC X value 'I'.

       01 FORMAT-EMPTY-LIGNE-STATS.
        05 PIC X value 'I'.
        05 PIC X(70).
        05 PIC X value 'I'.

       01 FORMAT-LIGNE-TABLE-STATS.
        05 PIC X value '+'.
        05 PIC x(70) value all '-'.
        05 pic x value '+'.

      * -------------------------------------------------

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           PERFORM 10000-INIT-PGM
           PERFORM 20000-TRAITEMENT
           PERFORM 30000-END-PGM
           .

       10000-INIT-PGM.
           OPEN INPUT f-assures-in f-mvt f-error
           OPEN OUTPUT f-assures-out f-etatano
           perform 11000-CHECK-INIT-FILE
           perform 18000-READ-ASSURES-IN
           perform 19000-READ-MVT

           perform get-current-date
           perform write-ano-header
           .

       11000-CHECK-INIT-FILE.
           IF CR-ASSURES-IN > 0
             DISPLAY 'ERROR ASSUR3 : ' CR-ASSURES-IN 
             perform 30000-END-PGM
           END-IF 
           IF CR-MVT  > 0
             DISPLAY 'ERROR MVT : ' CR-MVT 
             perform 30000-END-PGM
           END-IF
           IF CR-ERRVS > 0
             DISPLAY 'ERROR ERRVS : ' CR-ERRVS
             perform 30000-END-PGM
           END-IF
           IF CR-ASSURES-OUT  > 0
             DISPLAY 'ERROR ASSUR4 : ' CR-ASSURES-OUT 
           END-IF 
           IF CR-ETATANO > 0
             DISPLAY 'ERROR ETATANO : ' CR-ETATANO 
           END-IF
           IF CR-ERRVS > 0
             DISPLAY 'ERROR ETATANO : ' CR-ERRVS 
           END-IF
           .
       
       GET-CURRENT-DATE.
           ACCEPT DATE-F FROM DATE YYYYMMDD
           ACCEPT TIME-F FROM TIME
           ACCEPT WEEKDAY-F FROM DAY-OF-WEEK
           .

       write-ano-header.
           MOVE CORRESPONDING DATE-F TO FORMAT-MAJ-ANO

           WRITE etatano from FORMAT-HEADER-TITLE-ANO
           WRITE etatano from FORMAT-EMPTY-LIGNE-ANO
           WRITE etatano from FORMAT-MAJ-ANO
           WRITE etatano from FORMAT-GROUPE-ANO
           WRITE etatano from FORMAT-EMPTY-LIGNE-ANO
           WRITE etatano from FORMAT-LIGNE-TAB-ANO
           .
       
       write-ano-footer.
           write etatano from FORMAT-LIGNE-TAB-ANO
           .

       18000-READ-ASSURES-IN.
           READ f-assures-in

           IF NOT EOF-ASSURES-IN 
              ADD 1 TO CPT-ASSURES-IN
           END-IF
           .
       
       19000-READ-MVT.
           READ f-mvt

           IF NOT EOF-MVT
              ADD 1 TO CPT-MVT
           END-IF
           .

       20000-TRAITEMENT.
           perform until EOF-MVT

             perform until MAT-MVT <= MAT-A3 OR EOF-ASSURES-IN
               perform 21000-WRITE-ASSURES-NO-MVT
               perform 18000-READ-ASSURES-IN
             END-PERFORM

             IF (EOF-ASSURES-IN)
               perform 23000-FINISH-MVT
             ELSE 
               perform 22000-EVALUATE-CODE-MVT
               perform 19000-READ-MVT
             END-IF

           END-PERFORM
           .
       
       22700-CALL-ANO-PGM-ANO.
           CALL ano-pgm USING
                BY REFERENCE ERR-CODE
                BY REFERENCE LIB-MESS
                
           perform 22710-WRITE-ETAT-ANO

           ADD 1 TO CPT-ANO-T(ERR-CODE)
           .
       
       31700-CALL-ANO-PGM-STATS.
           CALL ano-pgm USING
                BY REFERENCE I
                BY REFERENCE message-erreur
           .
       
       22000-EVALUATE-CODE-MVT.
           EVALUATE CODE-MVT

             WHEN 'C'
               perform 22100-CHECK-WRITE-MVT

             WHEN 'M'
               perform 22200-CHECK-REWRITE-MVT

             WHEN 'S'
               perform 22300-CHECK-DELETE-MVT
               
             WHEN OTHER
               MOVE 1 TO ERR-CODE
               perform 22700-CALL-ANO-PGM-ANO

           END-EVALUATE
           .
       
       22710-WRITE-ETAT-ANO.
           MOVE MAT-MVT TO NUM-MAT
           MOVE CODE-MVT TO CODE-MVT-ANO
           write etatano
           .

       23000-FINISH-MVT.
           perform until EOF-MVT

             EVALUATE CODE-MVT

               WHEN 'C'
                 perform 22110-WRITE-ASSURES-OUT

               WHEN 'M'
                 MOVE 3 TO ERR-CODE
                 perform 22700-CALL-ANO-PGM-ANO

               WHEN 'S'
                 MOVE 4 TO ERR-CODE
                 perform 22700-CALL-ANO-PGM-ANO

               WHEN OTHER
                 MOVE 1 TO ERR-CODE
                 perform 22700-CALL-ANO-PGM-ANO

             END-EVALUATE

             perform 19000-READ-MVT

           END-PERFORM
           .

       22100-CHECK-WRITE-MVT.
           IF (MAT-A3 NOT = MAT-MVT)
             perform 22110-WRITE-ASSURES-OUT
           ELSE
             MOVE 2 TO ERR-CODE
             perform 22700-CALL-ANO-PGM-ANO
           END-IF 
           .
       
       22200-CHECK-REWRITE-MVT.
           IF (MAT-A3 = MAT-MVT)
             perform 22210-REWRITE-ASSURES-OUT
           ELSE
             MOVE 3 TO ERR-CODE
             perform 22700-CALL-ANO-PGM-ANO
           END-IF
           .
       
       22300-CHECK-DELETE-MVT.
           IF (MAT-A3 = MAT-MVT)
             perform 22310-DELETE-ASSURES-OUT
             perform 18000-READ-ASSURES-IN
           ELSE
             MOVE 4 TO ERR-CODE
             perform 22700-CALL-ANO-PGM-ANO
           END-IF
           .
           

       21000-WRITE-ASSURES-NO-MVT.
           write assures-out from e-assures-in
                      
           ADD 1 TO CPT-ASSURES-OUT
           .
       
       22110-WRITE-ASSURES-OUT.
           MOVE MAT-MVT TO MAT-A4 
           MOVE NOM-PRE-MVT TO NOM-PRE-A4 
           MOVE RUE-MVT TO RUE-A4
           MOVE CP-MVT TO CP-A4
           MOVE VILLE-MVT TO VILLE-A4
           MOVE TYPE-VEHICULE TO TYPE-V-A4 
           MOVE PRIME-MVT TO PRIME-A4
           MOVE BM-MVT TO BM-A4
           MOVE TAUX-MVT TO TAUX-A4
           write assures-out

           ADD 1 TO CPT-CREATE
           .
       
       22210-REWRITE-ASSURES-OUT.
           MOVE MAT-MVT TO MAT-A4 
           MOVE NOM-PRE-MVT TO NOM-PRE-A4 
           MOVE RUE-MVT TO RUE-A4
           MOVE CP-MVT TO CP-A4
           MOVE VILLE-MVT TO VILLE-A4
           MOVE TYPE-VEHICULE TO TYPE-V-A4 
           MOVE PRIME-MVT TO PRIME-A4
           MOVE BM-MVT TO BM-A4
           MOVE TAUX-MVT TO TAUX-A4
           write assures-out

           ADD 1 TO CPT-REWRITE
           .

       22310-DELETE-ASSURES-OUT.
           ADD 1 TO CPT-DELETE
           .

       30000-END-PGM.
           perform write-ano-footer
           close f-assures-in f-assures-out f-mvt f-etatano
           perform 31000-DISPLAY-STATS

           STOP RUN
           .

       31000-DISPLAY-STATS.
           perform VARYING I FROM 1 by 1 until I > 4
              ADD CPT-ANO-T(I) TO CPT-ANOMALIE 
           END-PERFORM
           
           perform 31100-DISPLAY-STATS-HEADER

           perform VARYING I from 5 by 1 until I > 15
              perform 31700-CALL-ANO-PGM-STATS
              MOVE CPT-STATS(I) TO CPTI-F
              DISPLAY FORMAT-ENRGMT-STATS
           END-PERFORM

           perform 31200-DISPLAY-STATS-FOOTER
           .

       31100-DISPLAY-STATS-HEADER.
           MOVE CORRESPONDING DATE-F TO FORMAT-HEADER-STATS-1
           MOVE weekday(WEEKDAY-F) TO WEEKDAY-S 
           MOVE CORRESPONDING TIME-F TO FORMAT-HEADER-STATS-2

           DISPLAY FORMAT-HEADER-STATS-1
           DISPLAY FORMAT-HEADER-STATS-2
           DISPLAY FORMAT-EMPTY-LIGNE-ANO 
           DISPLAY FORMAT-LIGNE-TABLE-STATS
           DISPLAY FORMAT-TITLE-STATS
           DISPLAY FORMAT-LIGNE-TABLE-STATS 
           DISPLAY FORMAT-EMPTY-LIGNE-STATS
           .

       31200-DISPLAY-STATS-FOOTER.
           DISPLAY FORMAT-EMPTY-LIGNE-STATS
           DISPLAY FORMAT-LIGNE-TABLE-STATS
           .