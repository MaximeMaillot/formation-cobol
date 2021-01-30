//API11CLK JOB 'SDJ FORMATION',MSGLEVEL=(1,1),MSGCLASS=H,CLASS=A,
//   NOTIFY=&SYSUID,TIME=(,10),RESTART=CREATE
//JOBLIB   DD DSN=API11.COBOL.LOAD,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//*
//DELETE EXEC PGM=IEFBR14
//DRECAP    DD  DSN=API11.ACTIF.RECAP,
//           DISP=(SHR,DELETE,DELETE)
//DSTATSR   DD  DSN=API11.ACTIF.STATSR,
//           DISP=(SHR,DELETE,DELETE)
//*
//CREATE EXEC PGM=IEFBR14
//CRECAP    DD DSN=API11.ACTIF.RECAP, 
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1,0),RLSE),UNIT=SYSDA,
//            DCB=(LRECL=80,BLKSIZE=800,RECFM=FB)
//CSTATSR   DD DSN=API11.ACTIF.STATSR, 
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1,0),RLSE),UNIT=SYSDA,
//            DCB=(LRECL=80,BLKSIZE=800,RECFM=FB)
//*
//APPEL EXEC IGYWCL,PARM.COBOL=(ADV,OBJECT,LIB,TEST,APOST)
//COBOL.SYSIN  DD DISP=SHR,DSN=API11.SOURCE.COBOL(INF11C11)
//COBOL.SYSLIB DD DISP=SHR,DSN=CEE.SCEESAMP
//LKED.SYSLMOD DD DSN=API11.COBOL.LOAD,DISP=(SHR,KEEP,KEEP)
//LKED.SYSIN   DD *
 NAME INF11C11(R)
/*
//*
//STEP1    EXEC PGM=INF11C11
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSIN    DD DUMMY
//DREGION  DD DSN=API11.SOURCE.COBOL(TREGION),DISP=SHR
//DMVT      DD DSN=API11.SOURCE.COBOL(TMVT),DISP=SHR
//DRECAP    DD DSN=API11.ACTIF.RECAP,DISP=SHR
//DSTATSR  DD DSN=API11.ACTIF.STATSR,DISP=SHR
