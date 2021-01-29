       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB8.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-CODE ASSIGN
            DDCODE FILE STATUS IS CR-CODE.
      *********************************
      *    D A T A   D I V I S I O N
      *********************************
       DATA DIVISION.

       FILE SECTION.
       fd F-CODE
           BLOCK CONTAINS 0
           DATA RECORD IS E-CODE.
       01 E-CODE                   PIC X(80).

       WORKING-STORAGE SECTION.
       01 CR-CODE                  PIC 99.
       77 EOF-TRUE                 PIC X VALUE "Y".
       77 EOF                      PIC X VALUE "F".
      * -------- Structure ASSURES -----
       01 W-CODE.
         05                        PIC X(6).
         05 SPECIAL-CHAR           PIC X(1).
         05 ZONE-A                 PIC X(4).
         05 ZONE-B                 PIC X(69).

       01 CPT.
         05 CPT-LIGNE-PROG         PIC 9(4) VALUE 0.
         05 CPT-LIGNE-COMMENT      PIC 9(4) VALUE 0.
         05 CPT-LIGNE-VIDE         PIC 9(4) VALUE 0.
       01 PERCENT.
         05 PERCENT-COMMENT        PIC 9(2)V9(2).
         05 PERCENT-VIDE           PIC 9(2)V9(2).

       01 FORMAT-HEADER.
         05 PIC X(10).
         05 PIC X(70) VALUE 'Statistique sur les programmes'.
       01 FORMAT-LIGNE.
         05 PIC X(10).
         05 PIC X(3) VALUE ' - '.
         05 LIGNE-N PIC X(30).
         05 PIC X(3) VALUE ' : '.
         05 CPT-LIGNE-N PIC 9(4).
       01 FORMAT-PERCENT.
         05 PIC X(10).
         05 PIC X(3) VALUE ' - '.
         05 PERCENT-N PIC X(30).
         05 PIC X(3) VALUE ' : '.
         05 PERCENT-EDIT PIC Z9,99.
         05 PIC X(1) VALUE '%'.

      ****************************************************************
      * P R O C E D U R E   D I V I S I O N
      ****************************************************************
       PROCEDURE DIVISION.
           OPEN INPUT F-CODE
           PERFORM DISPLAY-HEADER
           PERFORM UNTIL EOF = EOF-TRUE
             READ F-CODE INTO W-CODE
               AT END
                 MOVE EOF-TRUE TO EOF
               NOT AT END
                 PERFORM COUNT-LIGNE
             END-READ
           END-PERFORM
           CLOSE F-CODE
           PERFORM CALCUL-PERCENT
           PERFORM DISPLAY-STATS
           STOP RUN.
       COUNT-LIGNE.
           ADD 1 TO CPT-LIGNE-PROG
           IF W-CODE = SPACE
             ADD 1 TO CPT-LIGNE-VIDE
           END-IF
           IF SPECIAL-CHAR = '*'
             ADD 1 TO CPT-LIGNE-COMMENT
           END-IF
           .
       CALCUL-PERCENT.
           COMPUTE PERCENT-COMMENT =
            CPT-LIGNE-COMMENT * 100 / CPT-LIGNE-PROG
           COMPUTE PERCENT-VIDE =
            CPT-LIGNE-VIDE * 100 / CPT-LIGNE-PROG
           .
       DISPLAY-HEADER.
           DISPLAY FORMAT-HEADER
           .
       DISPLAY-STATS SECTION.
        DISPLAY-STATS-LIGNE.
           MOVE 'Nombre de lignes du programme' TO LIGNE-N
           MOVE CPT-LIGNE-PROG TO CPT-LIGNE-N
           DISPLAY FORMAT-LIGNE
           MOVE 'Nombre de ligne en commentaires' TO LIGNE-N
           MOVE CPT-LIGNE-COMMENT TO CPT-LIGNE-N
           DISPLAY FORMAT-LIGNE
           MOVE 'Nombre de ligne vides' TO LIGNE-N
           MOVE CPT-LIGNE-VIDE TO CPT-LIGNE-N
           DISPLAY FORMAT-LIGNE
           .
        DISPLAY-STATS-PERCENT.
           MOVE 'Pourcentages de commentaires' TO PERCENT-N
           MOVE PERCENT-COMMENT TO PERCENT-EDIT
           DISPLAY FORMAT-PERCENT
           MOVE 'Pourcentage de lignes blanches' TO PERCENT-N
           MOVE PERCENT-VIDE TO PERCENT-EDIT
           DISPLAY FORMAT-PERCENT
           .
