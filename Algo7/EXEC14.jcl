//API11E14 JOB 'SDJ FORMATION',MSGLEVEL=(1,1),MSGCLASS=H,CLASS=A,
//   NOTIFY=&SYSUID,TIME=(,5)
//JOBLIB   DD DSN=API11.COBOL.LOAD,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//*
//APPEL EXEC IGYWCL,PARM.COBOL=(ADV,OBJECT,LIB,TEST,APOST)
//COBOL.SYSIN  DD DISP=SHR,DSN=API11.SOURCE.COBOL(INF14C11)
//COBOL.SYSLIB DD DISP=SHR,DSN=CEE.SCEESAMP
//LKED.SYSLMOD DD DSN=API11.COBOL.LOAD,DISP=(SHR,KEEP,KEEP)
//LKED.SYSIN   DD *
 NAME INF14C11(R)
/*
//*
//STEP1    EXEC PGM=INF14C11
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSIN    DD DUMMY
//DETUD   DD DSN=API11.COB.ETUDIANT,DISP=SHR
