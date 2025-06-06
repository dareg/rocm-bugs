MODULE YY

IMPLICIT NONE

INTEGER, PARAMETER :: JPIM = 4, JPRB = 8

SAVE

INTEGER(KIND=JPIM), PARAMETER     :: JPMAX_NAM_LEN=10     
INTEGER(KIND=JPIM), PARAMETER     :: JPMAX_REGFILE_LEN=128
INTEGER(KIND=JPIM), PARAMETER     :: JPMAX_NPREDS=4       
INTEGER(KIND=JPIM), PARAMETER     :: JPMAX_REGFILES=60    

TYPE AMV_PC_INFO                 
INTEGER(KIND=JPIM)                              :: SAT_ID   
REAL(KIND=JPRB)                                 :: SAT_LON  
INTEGER(KIND=JPIM)                              :: COMP_METHOD 
INTEGER(KIND=JPIM)                              :: NPREDS   
CHARACTER(LEN=JPMAX_NAM_LEN), DIMENSION(JPMAX_NPREDS) :: PRED_NAME
REAL(KIND=JPRB), DIMENSION(0:JPMAX_NPREDS)      :: COEF     
END TYPE AMV_PC_INFO

TYPE(AMV_PC_INFO), ALLOCATABLE, DIMENSION(:)            :: TPC_INFO

INTEGER(KIND=JPIM)                :: NPC_INFO  

END MODULE YY

SUBROUTINE AMV_REASSIGN(KDLEN,KLEN,KGRP,PLAT,PLON,PDP)  

USE YY

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KDLEN 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEN 
INTEGER(KIND=JPIM),INTENT(IN)    :: KGRP 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAT(KDLEN) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLON(KDLEN) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PDP(KDLEN) 


REAL(KIND=JPRB), ALLOCATABLE, DIMENSION(:,:)   :: ZPREDS    
REAL(KIND=JPRB), DIMENSION(KDLEN)              :: ZP        
REAL(KIND=JPRB), DIMENSION(KDLEN)              :: ZEXPARG

INTEGER(KIND=JPIM)                             :: JPRED, JCOR

WHERE ( ZP(1:KLEN) <= 60000.0_JPRB .AND. ABS(PLAT(1:KLEN)) >= 20.0_JPRB )
  ZEXPARG(1:KLEN) = LOG(ZP(1:KLEN)) + TPC_INFO(JCOR)%COEF(0)     &
   & + MATMUL(ZPREDS(1:KLEN,:),TPC_INFO(JCOR)%COEF(1:TPC_INFO(JCOR)%NPREDS))  
ENDWHERE

END SUBROUTINE AMV_REASSIGN
