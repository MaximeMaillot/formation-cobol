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
           SELECT f-mvt ASSIGN AS-FMVT
            ORGANIZATION SEQUENTIAL
            FILE STATUS IS CR-MVT.

           SELECT f-assures4 ASSIGN ASSUR4
            ORGANIZATION IS INDEXED
            ACCESS MODE RANDOM
            RECORD KEY IS MAT-X4 
            FILE STATUS IS CR-ASSURES4.
           
           SELECT f-etatano ASSIGN ETATANO
            FILE STATUS IS CR-ETATANO.

      *********************************
      *    D A T A   D I V I S I O N
      *********************************
       DATA DIVISION.
       FILE SECTION.
       fd f-mvt
           DATA RECORD IS e-mvt.
       01 e-mvt.
           COPY CMVTPROJ.
       
       fd f-assures4.
       01 ASSURES4.
           COPY CASSURE4.

       fd f-etatano.
       01 etatano.
           COPY CANO.
       

       WORKING-STORAGE SECTION.
       
       01 CR-MVT                         PIC 99.
           88 EOF-MVT               VALUE 10.
       01 CR-ASSURES4                    PIC 99.
         88 DUPLICATE-KEY           VALUE 22.
         88 KEY-NOT-FOUND           VALUE 23.
       01 CR-ETATANO                     PIC 99.

      * --------------- COMPTEURS --------------

       01 CPT.
         02 CPT-FILLER.
           05                            PIC 99 OCCURS 4.
           
         02 CPT-READ.
           05                PIC 99.
           05 CPT-MVT                    PIC 99.
           05 CPT-CREATE                 PIC 99.
           05                PIC 99.
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

      * --------------------------------------------------- 

      * ------------- GESTION DATE --------------

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

      * ----------------------------------------- 

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

       77 ano-pgm                    PIC X(7) VALUE 'ETATANO'.
       01 ano-pgm-param.
         05 L-ERROR-CODE             PIC 9(3).
         05 err-label                PIC x(60).
         05 FLAG                     PIC 9.
           88 flag-open value 0.
           88 flag-continue value 5.
           88 flag-close value 9.
         05 CR-ERRVS                 PIC 99.
       
       01 ano-pgm-param-stats redefines ano-pgm-param.
         05 I                        PIC 9(3).
         05                          PIC X(60).
         05                          PIC 9.
         05                          PIC 99.
        


      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
       00000-DEBUT.
           PERFORM 10000-INIT-PGM
           PERFORM 20000-TRAITEMENT
           PERFORM 30000-END-PGM

           stop run
           .

       10000-INIT-PGM.
           OPEN INPUT f-mvt
           OPEN OUTPUT f-etatano
           OPEN I-O f-assures4
           perform 11000-CHECK-INIT-FILE
           perform 19000-READ-MVT

           perform 12000-get-current-date
           perform 13000-write-ano-header
           .

       11000-CHECK-INIT-FILE.
           IF CR-MVT  > 0
             DISPLAY 'ERROR MVT : ' CR-MVT 
             perform 11100-ABORT-PGM 
           END-IF

           IF CR-ASSURES4  > 0
             DISPLAY 'ERROR ASSUR4 : ' CR-ASSURES4
             perform 11100-ABORT-PGM 
           END-IF 

           IF CR-ETATANO > 0
             DISPLAY 'ERROR ETATANO : ' CR-ETATANO
             perform 11100-ABORT-PGM 
           END-IF

           MOVE 0 TO FLAG
           CALL ano-pgm USING ano-pgm-param
           IF CR-ERRVS > 0
             DISPLAY 'ERROR ERRVS : ' CR-ERRVS 
             perform 11100-ABORT-PGM 
           END-IF
           .
       
       12000-GET-CURRENT-DATE.
           ACCEPT DATE-F FROM DATE YYYYMMDD
           ACCEPT TIME-F FROM TIME
           ACCEPT WEEKDAY-F FROM DAY-OF-WEEK
           .

       13000-write-ano-header.
           MOVE CORRESPONDING DATE-F TO FORMAT-MAJ-ANO

           WRITE etatano from FORMAT-HEADER-TITLE-ANO
           WRITE etatano from FORMAT-EMPTY-LIGNE-ANO
           WRITE etatano from FORMAT-MAJ-ANO
           WRITE etatano from FORMAT-GROUPE-ANO
           WRITE etatano from FORMAT-EMPTY-LIGNE-ANO
           WRITE etatano from FORMAT-LIGNE-TAB-ANO
           .
       
       32000-write-ano-footer.
           write etatano from FORMAT-LIGNE-TAB-ANO
           .
       
       19000-READ-MVT.
           READ f-mvt

           IF NOT EOF-MVT
              ADD 1 TO CPT-MVT
           END-IF
           .

       20000-TRAITEMENT.
           MOVE 5 TO FLAG

           perform until EOF-MVT
            EVALUATE CODE-MVT

             WHEN 'C'
               perform 21000-WRITE-ASSURES4
               IF CR-ASSURES4 > 0
                 MOVE 2 TO L-ERROR-CODE
                 perform 27000-CALL-ANO-PGM-ANO
               ELSE
                 ADD 1 TO CPT-CREATE                  
               END-IF

             WHEN 'M'
               perform 22000-REWRITE-ASSURES4
               IF CR-ASSURES4 > 0
                 MOVE 3 TO L-ERROR-CODE
                 perform 27000-CALL-ANO-PGM-ANO
               ELSE
                 ADD 1 TO CPT-REWRITE 
               END-IF

             WHEN 'S'
              perform 23000-DELETE-ASSURES4
              IF CR-ASSURES4 > 0
                 MOVE 4 TO L-ERROR-CODE
                 perform 27000-CALL-ANO-PGM-ANO
              ELSE
                 ADD 1 TO CPT-DELETE
              END-IF

             WHEN OTHER
               MOVE 1 TO L-ERROR-CODE
               perform 27000-CALL-ANO-PGM-ANO

            END-EVALUATE
           
           perform 19000-READ-MVT
           END-PERFORM
           .
       
       27000-CALL-ANO-PGM-ANO.
           CALL ano-pgm USING BY REFERENCE ano-pgm-param
                
           perform 27100-WRITE-ETAT-ANO

           ADD 1 TO CPT-ANO-T(L-ERROR-CODE)
           .
       
       31700-CALL-ANO-PGM-STATS.
           CALL ano-pgm USING BY REFERENCE ano-pgm-param-stats 
           .
       
       27100-WRITE-ETAT-ANO.
           MOVE MAT-MVT TO NUM-MAT
           MOVE CODE-MVT TO CODE-MVT-ANO
           MOVE err-label to LIB-MESS  

           write etatano
           .
       
       21000-WRITE-ASSURES4.
           MOVE MAT-MVT TO MAT-A4 
           MOVE NOM-PRE-MVT TO NOM-PRE-A4 
           MOVE RUE-MVT TO RUE-A4
           MOVE CP-MVT TO CP-A4
           MOVE VILLE-MVT TO VILLE-A4
           MOVE TYPE-VEHICULE TO TYPE-V-A4 
           MOVE PRIME-MVT TO PRIME-A4
           MOVE BM-MVT TO BM-A4
           MOVE TAUX-MVT TO TAUX-A4

           write ASSURES4
           .
       
       22000-REWRITE-ASSURES4.
           MOVE MAT-MVT TO MAT-A4
           MOVE NOM-PRE-MVT TO NOM-PRE-A4 
           MOVE RUE-MVT TO RUE-A4
           MOVE CP-MVT TO CP-A4
           MOVE VILLE-MVT TO VILLE-A4
           MOVE TYPE-VEHICULE TO TYPE-V-A4 
           MOVE PRIME-MVT TO PRIME-A4
           MOVE BM-MVT TO BM-A4
           MOVE TAUX-MVT TO TAUX-A4

           REWRITE ASSURES4
           .

       23000-DELETE-ASSURES4.
           DELETE f-assures4
           .

       30000-END-PGM.
           close f-assures4 f-mvt
           perform 32000-write-ano-footer

           close f-etatano
           perform 31000-DISPLAY-STATS

           MOVE 9 TO FLAG
           CALL ano-pgm USING ano-pgm-param
           .

       31000-DISPLAY-STATS.
           perform VARYING I FROM 1 by 1 until I > 4
              ADD CPT-ANO-T(I) TO CPT-ANOMALIE
           END-PERFORM
           
           perform 31100-DISPLAY-STATS-HEADER

           perform VARYING I from 5 by 1 until I > 15
              perform 31700-CALL-ANO-PGM-STATS
              MOVE CPT-STATS(I) TO CPTI-F
              MOVE err-label TO message-erreur
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
       
       11100-ABORT-PGM.
           MOVE 9 TO FLAG
           CALL ano-pgm USING BY REFERENCE ano-pgm-param
           close f-assures4 f-mvt f-etatano

           STOP RUN
           .