//API11E05 JOB 'SDJ FORMATION',MSGLEVEL=(1,1),MSGCLASS=H,CLASS=A,
//   NOTIFY=&SYSUID,TIME=(,10)
//JOBLIB   DD DSN=API11.COBOL.LOAD,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//APPEL EXEC IGYWCL,PARM.COBOL=(ADV,OBJECT,LIB,TEST,APOST)
//COBOL.SYSIN  DD DISP=SHR,DSN=API11.SOURCE.COBOL(INF05C11)
//COBOL.SYSLIB DD DISP=SHR,DSN=CEE.SCEESAMP
//LKED.SYSLMOD DD DSN=API11.COBOL.LOAD,DISP=(SHR,KEEP,KEEP)
//LKED.SYSIN DD *
 NAME INF05C11(R)
/*
//*
//STEP1    EXEC PGM=INF05C11
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSIN    DD DUMMY
//DDMVT  DD DSN=API11.COB.MVT,DISP=SHR