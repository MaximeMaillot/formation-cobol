//API11E11 JOB 'SDJ FORMATION',MSGLEVEL=(1,1),MSGCLASS=H,CLASS=A,
//   NOTIFY=&SYSUID,TIME=(,10)
//JOBLIB   DD DSN=API11.COBOL.LOAD,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//*
//APPEL EXEC IGYWCL,PARM.COBOL=(ADV,OBJECT,LIB,TEST,APOST)
//COBOL.SYSIN  DD DISP=SHR,DSN=API11.SOURCE.COBOL(INFCOC11)
//COBOL.SYSLIB DD DISP=SHR,DSN=CEE.SCEESAMP
//             DD DISP=SHR,DSN=API0.SOURCE.COPY
//LKED.SYSLMOD DD DSN=API11.COBOL.LOAD,DISP=(SHR,KEEP,KEEP)
//LKED.SYSIN   DD *
 NAME INFCOC11(R)
/*
//*
//STEP1    EXEC PGM=INFCOC11
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSIN    DD DUMMY