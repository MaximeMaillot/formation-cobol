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

           SELECT f-error ASSIGN ERRVS
            ORGANIZATION IS INDEXED
            ACCESS MODE IS RANDOM
            RECORD KEY IS error-key-x
            FILE STATUS IS CR-ERRVS.
           
           SELECT f-etatano ASSIGN ETATANO
            FILE STATUS IS CR-ETATANO.

      * Voir pour ouvrir les fichiers utilisés dans les programmmes
      * secondaires dans le programme principal

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
       
       fd f-error is external
           DATA RECORD IS e-error.
       01 e-error.
           02 error-key-9                pic 9(3).
           02 error-key-x 
            REDEFINES error-key-9        PIC x(3).
           02 err-message                pic x(60).
           02                            PIC X(17).

       fd f-etatano is external.
       01 etatano                        PIC X(80).
       

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

       77 I                              PIC 99.

       77 ERR-CODE                       PIC 99.

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
           OPEN OUTPUT f-assures-out
           OPEN EXTEND f-etatano 
           perform 11000-CHECK-INIT-FILE
           perform READ-ASSURES-IN
           perform READ-MVT
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
           .
       
       READ-ASSURES-IN.
           READ f-assures-in

           IF NOT EOF-ASSURES-IN 
              ADD 1 TO CPT-ASSURES-IN
           END-IF
           .
       
       READ-MVT.
           READ f-mvt

           IF NOT EOF-MVT
              ADD 1 TO CPT-MVT
           END-IF
           .

       20000-TRAITEMENT.
           perform until EOF-MVT

             perform until MAT-MVT <= MAT-A3 OR EOF-ASSURES-IN
               perform WRITE-ASSURES-NO-MVT
               perform READ-ASSURES-IN
             END-PERFORM

             IF (EOF-ASSURES-IN)
               perform FINISH-MVT
             ELSE 
               perform EVALUATE-CODE-MVT
               perform READ-MVT
             END-IF

           END-PERFORM
           .
       
       CALL-ANO-PGM.
           CALL ano-pgm USING
                BY REFERENCE CODE-MVT MAT-MVT ERR-CODE
                
           ADD 1 TO CPT-ANO-T(ERR-CODE)
           .
       
       EVALUATE-CODE-MVT.
           EVALUATE CODE-MVT
             WHEN 'C'
               perform CHECK-WRITE-MVT
             WHEN 'M'
               perform CHECK-REWRITE-MVT
             WHEN 'S'
               perform CHECK-DELETE-MVT
             WHEN OTHER
               MOVE 1 TO ERR-CODE
               perform CALL-ANO-PGM
           END-EVALUATE
           .

       FINISH-MVT.
           perform until EOF-MVT

             EVALUATE CODE-MVT
               WHEN 'C'
                 perform WRITE-ASSURES-OUT
               WHEN 'M'
                 MOVE 3 TO ERR-CODE
                 perform CALL-ANO-PGM
               WHEN 'S'
                 MOVE 4 TO ERR-CODE
                 perform CALL-ANO-PGM
               WHEN OTHER
                 MOVE 1 TO ERR-CODE
                 perform CALL-ANO-PGM
             END-EVALUATE
             perform READ-MVT

           END-PERFORM
           .

       CHECK-WRITE-MVT.
           IF (MAT-A3 NOT = MAT-MVT)
             perform WRITE-ASSURES-OUT
           ELSE
             MOVE 2 TO ERR-CODE
             perform CALL-ANO-PGM
           END-IF 
           .
       
       CHECK-REWRITE-MVT.
           IF (MAT-A3 = MAT-MVT)
             perform REWRITE-ASSURES-OUT
           ELSE
             MOVE 3 TO ERR-CODE
             perform CALL-ANO-PGM
           END-IF
           .
       
       CHECK-DELETE-MVT.
           IF (MAT-A3 = MAT-MVT)
             perform DELETE-ASSURES-OUT
           ELSE
             MOVE 4 TO ERR-CODE
             perform CALL-ANO-PGM
           END-IF
           .

       WRITE-ASSURES-NO-MVT.
           write assures-out from e-assures-in
           
           ADD 1 TO CPT-ASSURES-OUT
           .
       
       WRITE-ASSURES-OUT.
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
       
       REWRITE-ASSURES-OUT.
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

       DELETE-ASSURES-OUT.
           ADD 1 TO CPT-DELETE
           .

       30000-END-PGM.
           close f-assures-in f-assures-out f-mvt
           perform DISPLAY-STATS
           STOP RUN
           .

       DISPLAY-STATS.
           perform VARYING I FROM 1 by 1 until I > 4
              ADD CPT-ANO-T(I) TO CPT-ANOMALIE 
           END-PERFORM

           perform VARYING I from 5 by 1 until I > 15
              MOVE I to error-key-9
              read f-error
              DISPLAY err-message CPT-STATS(I)
           END-PERFORM
           .