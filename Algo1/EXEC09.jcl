//API11E09 JOB 'SDJ FORMATION',MSGLEVEL=(1,1),MSGCLASS=H,CLASS=A,
//   NOTIFY=&SYSUID,TIME=(,2),RESTART=CREATE
//JOBLIB   DD DSN=API11.COBOL.LOAD,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//*
//DELETE EXEC PGM=IEFBR14
//DCOMPTA   DD  DSN=API11.ACTIF.COMPTA,
//           DISP=(SHR,DELETE,DELETE)
//DDEPASS   DD  DSN=API11.ACTIF.DEPASS,
//           DISP=(SHR,DELETE,DELETE)
//DSTATS    DD  DSN=API11.ACTIF.STATS,
//           DISP=(SHR,DELETE,DELETE)
//*
//CREATE EXEC PGM=IEFBR14
//CCOMPTA   DD DSN=API11.ACTIF.COMPTA,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1,0),RLSE),UNIT=SYSDA,
//            DCB=(LRECL=80,BLKSIZE=800,RECFM=FB)
//CDEPASS   DD DSN=API11.ACTIF.DEPASS,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1,0),RLSE),UNIT=SYSDA,
//            DCB=(LRECL=80,BLKSIZE=800,RECFM=FB)
//CSTATS    DD DSN=API11.ACTIF.STATS,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1,0),RLSE),UNIT=SYSDA,
//            DCB=(LRECL=80,BLKSIZE=800,RECFM=FB)
//*
//APPEL EXEC IGYWCL,PARM.COBOL=(ADV,OBJECT,LIB,TEST,APOST)
//COBOL.SYSIN  DD DISP=SHR,DSN=API11.SOURCE.COBOL(INF09C11)
//COBOL.SYSLIB DD DISP=SHR,DSN=CEE.SCEESAMP
//LKED.SYSLMOD DD DSN=API11.COBOL.LOAD,DISP=(SHR,KEEP,KEEP)
//LKED.SYSIN   DD *
 NAME INF09C11(R)
/*
//*
//STEP1    EXEC PGM=INF9C11
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSIN    DD DUMMY
//DDSTOCK  DD DSN=API11.COB.FSTOCK,DISP=SHR
//DDCOMPTA DD DSN=API11.ACTIF.COMPTA,DISP=SHR
//DDDEPASS DD DSN=API11.ACTIF.DEPASS,DISP=SHR
//DDSTATS  DD DSN=API11.ACTIF.STATS,DISP=SHR
