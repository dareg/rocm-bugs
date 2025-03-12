
MODULE FIELD_5RD_DATA_MODULE


USE FIELD_CONSTANTS_MODULE
USE PARKIND1, ONLY : JPRM, JPRD, JPIM, JPLM

IMPLICIT NONE

PRIVATE


PUBLIC :: FIELD_5RD_COPY
PUBLIC :: FIELD_5RD_COPY_FUNC
PUBLIC :: FIELD_5RD_COPY_INTF

ABSTRACT INTERFACE
  SUBROUTINE FIELD_5RD_COPY_INTF (HST, DEV, MAP_DEVPTR, KDIR, QUEUE)
    IMPORT :: JPIM, JPRD
    REAL(KIND=JPRD), POINTER :: HST (:,:,:,:,:), DEV (:,:,:,:,:)
    LOGICAL,                       INTENT (IN) :: MAP_DEVPTR
    INTEGER (KIND=JPIM),           INTENT (IN) :: KDIR
    INTEGER (KIND=JPIM), OPTIONAL, INTENT (IN) :: QUEUE
  END SUBROUTINE
END INTERFACE

CONTAINS


  FUNCTION FIELD_5RD_COPY_FUNC (HST, DEV) RESULT (FUNC)

    USE FIELD_ABORT_MODULE

    PROCEDURE (FIELD_5RD_COPY_INTF), POINTER :: FUNC 

    REAL(KIND=JPRD), POINTER, OPTIONAL :: HST (:,:,:,:,:), DEV (:,:,:,:,:)

    INTEGER :: LAST_CONTIG_DIM
    INTEGER :: NEXT_CONTIG_DIM

    IF (PRESENT (HST)) THEN
      LAST_CONTIG_DIM = FIELD_5RD_GET_LAST_CONTIGUOUS_DIMENSION (HST, 0)
      NEXT_CONTIG_DIM = FIELD_5RD_GET_LAST_CONTIGUOUS_DIMENSION (HST, LAST_CONTIG_DIM+1)
    ELSE
      LAST_CONTIG_DIM = 5
      NEXT_CONTIG_DIM = 5
    ENDIF

    SELECT CASE (LAST_CONTIG_DIM)
      CASE (0)
        FUNC => FIELD_5RD_COPY_DIM0_CONTIGUOUS 
      CASE (1)
        FUNC => FIELD_5RD_COPY_DIM1_CONTIGUOUS 
      CASE (2)
        FUNC => FIELD_5RD_COPY_DIM2_CONTIGUOUS 
      CASE (3)
        FUNC => FIELD_5RD_COPY_DIM3_CONTIGUOUS 
      CASE (4)
        FUNC => FIELD_5RD_COPY_DIM4_CONTIGUOUS 
      CASE (5)
        FUNC => FIELD_5RD_COPY_DIM5_CONTIGUOUS 
      CASE DEFAULT
        CALL FIELD_ABORT ('INTERNAL ERROR: UNEXPECTED LAST_CONTIG_DIM')
    END SELECT

  END FUNCTION

  SUBROUTINE FIELD_5RD_COPY (HST, DEV, MAP_DEVPTR, KDIR, QUEUE)

    USE FIELD_ABORT_MODULE

    REAL(KIND=JPRD), POINTER :: HST (:,:,:,:,:), DEV (:,:,:,:,:)
    LOGICAL,                       INTENT (IN) :: MAP_DEVPTR
    INTEGER (KIND=JPIM),           INTENT (IN) :: KDIR
    INTEGER (KIND=JPIM), OPTIONAL, INTENT (IN) :: QUEUE

    PROCEDURE (FIELD_5RD_COPY_INTF), POINTER :: FUNC 

    FUNC => FIELD_5RD_COPY_FUNC (HST, DEV) 

    CALL FUNC (HST, DEV, MAP_DEVPTR, KDIR, QUEUE)

  END SUBROUTINE

  SUBROUTINE FIELD_5RD_COPY_DIM0_CONTIGUOUS (HST, DEV, MAP_DEVPTR, KDIR, QUEUE)
  
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : INT64
    REAL(KIND=JPRD), POINTER :: HST (:,:,:,:,:), DEV (:,:,:,:,:)
    LOGICAL,                       INTENT (IN) :: MAP_DEVPTR
    INTEGER (KIND=JPIM),           INTENT (IN) :: KDIR
    INTEGER (KIND=JPIM), OPTIONAL, INTENT (IN) :: QUEUE
    INTEGER (KIND=INT64)                       :: ISIZE
    INTEGER :: J, J1, J2, J3, J4, J5
    

    DO J5 = LBOUND (HST, 5), UBOUND (HST, 5)
      DO J4 = LBOUND (HST, 4), UBOUND (HST, 4)
        DO J3 = LBOUND (HST, 3), UBOUND (HST, 3)
          DO J2 = LBOUND (HST, 2), UBOUND (HST, 2)
            DO J1 = LBOUND (HST, 1), UBOUND (HST, 1)
#ifdef WITH_GPU_OFFLOAD
              IF(MAP_DEVPTR)THEN
    
                DEVPTR = 
    
              ELSE
    
                DEVPTR = 
    
              ENDIF
#endif
              ISIZE = KIND (HST)
              IF (KDIR == NH2D) THEN
#ifdef WITH_GPU_OFFLOAD
                IF(PRESENT(QUEUE))THEN
              
                ELSE
              
                ENDIF
#else
                DEV (J1 + LBOUND(DEV,1) - LBOUND (HST,1), J2 + LBOUND(DEV,2) - LBOUND (HST,2), J3 + LBOUND(DEV,3) - LBOUND&
                    & (HST,3), J4 + LBOUND(DEV,4) - LBOUND (HST,4), J5 + LBOUND(DEV,5) - LBOUND (HST,5)) = HST (J1, J2, J3, J4, J5)
#endif
              ELSEIF (KDIR == ND2H) THEN
#ifdef WITH_GPU_OFFLOAD
                IF(PRESENT(QUEUE))THEN
              
                ELSE
              
                ENDIF
#else
                HST (J1, J2, J3, J4, J5) = DEV (J1 + LBOUND(DEV,1) - LBOUND (HST,1), J2 + LBOUND(DEV,2) - LBOUND (HST,2), J3 +&
                    & LBOUND(DEV,3) - LBOUND (HST,3), J4 + LBOUND(DEV,4) - LBOUND (HST,4), J5 + LBOUND(DEV,5) - LBOUND (HST,5))
#endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
    ENDDO
    END SUBROUTINE

  SUBROUTINE FIELD_5RD_COPY_DIM1_CONTIGUOUS (HST, DEV, MAP_DEVPTR, KDIR, QUEUE)
  
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : INT64
    REAL(KIND=JPRD), POINTER :: HST (:,:,:,:,:), DEV (:,:,:,:,:)
    LOGICAL,                       INTENT (IN) :: MAP_DEVPTR
    INTEGER (KIND=JPIM),           INTENT (IN) :: KDIR
    INTEGER (KIND=JPIM), OPTIONAL, INTENT (IN) :: QUEUE
    INTEGER (KIND=INT64)                       :: ISIZE
    INTEGER :: J, J2, J3, J4, J5
    

    DO J5 = LBOUND (HST, 5), UBOUND (HST, 5)
      DO J4 = LBOUND (HST, 4), UBOUND (HST, 4)
        DO J3 = LBOUND (HST, 3), UBOUND (HST, 3)
          DO J2 = LBOUND (HST, 2), UBOUND (HST, 2)
#ifdef WITH_GPU_OFFLOAD
            IF(MAP_DEVPTR)THEN
   
              DEVPTR = 
   
            ELSE
   
              DEVPTR = 
   
            ENDIF
#endif
            ISIZE = SIZEOF ( HST(:, J2, J3, J4, J5) )
            IF (KDIR == NH2D) THEN
#ifdef WITH_GPU_OFFLOAD
              IF(PRESENT(QUEUE))THEN
             
              ELSE
             
              ENDIF
#else
              DEV (:, J2 + LBOUND(DEV,2) - LBOUND (HST,2), J3 + LBOUND(DEV,3) - LBOUND (HST,3), J4 + LBOUND(DEV,4) - LBOUND&
                  & (HST,4), J5 + LBOUND(DEV,5) - LBOUND (HST,5)) = HST (:, J2, J3, J4, J5)
#endif
            ELSEIF (KDIR == ND2H) THEN
#ifdef WITH_GPU_OFFLOAD
              IF(PRESENT(QUEUE))THEN
             
              ELSE
             
              ENDIF
#else
              HST (:, J2, J3, J4, J5) = DEV (:, J2 + LBOUND(DEV,2) - LBOUND (HST,2), J3 + LBOUND(DEV,3) - LBOUND (HST,3), J4 +&
                  & LBOUND(DEV,4) - LBOUND (HST,4), J5 + LBOUND(DEV,5) - LBOUND (HST,5))
#endif
            ENDIF
          ENDDO
        ENDDO
      ENDDO
    ENDDO
    END SUBROUTINE

  SUBROUTINE FIELD_5RD_COPY_DIM2_CONTIGUOUS (HST, DEV, MAP_DEVPTR, KDIR, QUEUE)
  
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : INT64
    REAL(KIND=JPRD), POINTER :: HST (:,:,:,:,:), DEV (:,:,:,:,:)
    LOGICAL,                       INTENT (IN) :: MAP_DEVPTR
    INTEGER (KIND=JPIM),           INTENT (IN) :: KDIR
    INTEGER (KIND=JPIM), OPTIONAL, INTENT (IN) :: QUEUE
    INTEGER (KIND=INT64)                       :: ISIZE
    INTEGER :: J, J3, J4, J5
    

    DO J5 = LBOUND (HST, 5), UBOUND (HST, 5)
      DO J4 = LBOUND (HST, 4), UBOUND (HST, 4)
        DO J3 = LBOUND (HST, 3), UBOUND (HST, 3)
#ifdef WITH_GPU_OFFLOAD
          IF(MAP_DEVPTR)THEN
  
            DEVPTR = 
  
          ELSE
  
            DEVPTR = 
  
          ENDIF
#endif
          ISIZE = SIZEOF ( HST(:, :, J3, J4, J5) )
          IF (KDIR == NH2D) THEN
#ifdef WITH_GPU_OFFLOAD
            IF(PRESENT(QUEUE))THEN
            
            ELSE
            
            ENDIF
#else
            DEV (:, :, J3 + LBOUND(DEV,3) - LBOUND (HST,3), J4 + LBOUND(DEV,4) - LBOUND (HST,4), J5 + LBOUND(DEV,5) - LBOUND&
                & (HST,5)) = HST (:, :, J3, J4, J5)
#endif
          ELSEIF (KDIR == ND2H) THEN
#ifdef WITH_GPU_OFFLOAD
            IF(PRESENT(QUEUE))THEN
            
            ELSE
            
            ENDIF
#else
            HST (:, :, J3, J4, J5) = DEV (:, :, J3 + LBOUND(DEV,3) - LBOUND (HST,3), J4 + LBOUND(DEV,4) - LBOUND (HST,4), J5 +&
                & LBOUND(DEV,5) - LBOUND (HST,5))
#endif
          ENDIF
        ENDDO
      ENDDO
    ENDDO
    END SUBROUTINE

  SUBROUTINE FIELD_5RD_COPY_DIM3_CONTIGUOUS (HST, DEV, MAP_DEVPTR, KDIR, QUEUE)
  
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : INT64
    REAL(KIND=JPRD), POINTER :: HST (:,:,:,:,:), DEV (:,:,:,:,:)
    LOGICAL,                       INTENT (IN) :: MAP_DEVPTR
    INTEGER (KIND=JPIM),           INTENT (IN) :: KDIR
    INTEGER (KIND=JPIM), OPTIONAL, INTENT (IN) :: QUEUE
    INTEGER (KIND=INT64)                       :: ISIZE
    INTEGER :: J, J4, J5
    

    DO J5 = LBOUND (HST, 5), UBOUND (HST, 5)
      DO J4 = LBOUND (HST, 4), UBOUND (HST, 4)
#ifdef WITH_GPU_OFFLOAD
        IF(MAP_DEVPTR)THEN
 
          DEVPTR = 
 
        ELSE
 
          DEVPTR = 
 
        ENDIF
#endif
        ISIZE = SIZEOF ( HST(:, :, :, J4, J5) )
        IF (KDIR == NH2D) THEN
#ifdef WITH_GPU_OFFLOAD
          IF(PRESENT(QUEUE))THEN
           
          ELSE
           
          ENDIF
#else
          DEV (:, :, :, J4 + LBOUND(DEV,4) - LBOUND (HST,4), J5 + LBOUND(DEV,5) - LBOUND (HST,5)) = HST (:, :, :, J4, J5)
#endif
        ELSEIF (KDIR == ND2H) THEN
#ifdef WITH_GPU_OFFLOAD
          IF(PRESENT(QUEUE))THEN
           
          ELSE
           
          ENDIF
#else
          HST (:, :, :, J4, J5) = DEV (:, :, :, J4 + LBOUND(DEV,4) - LBOUND (HST,4), J5 + LBOUND(DEV,5) - LBOUND (HST,5))
#endif
        ENDIF
      ENDDO
    ENDDO
    END SUBROUTINE

  SUBROUTINE FIELD_5RD_COPY_DIM4_CONTIGUOUS (HST, DEV, MAP_DEVPTR, KDIR, QUEUE)
  
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : INT64
    REAL(KIND=JPRD), POINTER :: HST (:,:,:,:,:), DEV (:,:,:,:,:)
    LOGICAL,                       INTENT (IN) :: MAP_DEVPTR
    INTEGER (KIND=JPIM),           INTENT (IN) :: KDIR
    INTEGER (KIND=JPIM), OPTIONAL, INTENT (IN) :: QUEUE
    INTEGER (KIND=INT64)                       :: ISIZE
    INTEGER :: J, J5
    

    DO J5 = LBOUND (HST, 5), UBOUND (HST, 5)
#ifdef WITH_GPU_OFFLOAD
      IF(MAP_DEVPTR)THEN

        DEVPTR = 

      ELSE

        DEVPTR = 

      ENDIF
#endif
      ISIZE = SIZEOF ( HST(:, :, :, :, J5) )
      IF (KDIR == NH2D) THEN
#ifdef WITH_GPU_OFFLOAD
        IF(PRESENT(QUEUE))THEN
          
        ELSE
          
        ENDIF
#else
        DEV (:, :, :, :, J5 + LBOUND(DEV,5) - LBOUND (HST,5)) = HST (:, :, :, :, J5)
#endif
      ELSEIF (KDIR == ND2H) THEN
#ifdef WITH_GPU_OFFLOAD
        IF(PRESENT(QUEUE))THEN
          
        ELSE
          
        ENDIF
#else
        HST (:, :, :, :, J5) = DEV (:, :, :, :, J5 + LBOUND(DEV,5) - LBOUND (HST,5))
#endif
      ENDIF
    ENDDO
    END SUBROUTINE

  SUBROUTINE FIELD_5RD_COPY_DIM5_CONTIGUOUS (HST, DEV, MAP_DEVPTR, KDIR, QUEUE)
  
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : INT64
    REAL(KIND=JPRD), POINTER :: HST (:,:,:,:,:), DEV (:,:,:,:,:)
    LOGICAL,                       INTENT (IN) :: MAP_DEVPTR
    INTEGER (KIND=JPIM),           INTENT (IN) :: KDIR
    INTEGER (KIND=JPIM), OPTIONAL, INTENT (IN) :: QUEUE
    INTEGER (KIND=INT64)                       :: ISIZE
    INTEGER :: J
    

#ifdef WITH_GPU_OFFLOAD
        IF(MAP_DEVPTR)THEN
 
          DEVPTR = 
 
        ELSE
 
          DEVPTR = 
 
        ENDIF
#endif
        ISIZE = SIZEOF ( HST(:, :, :, :, :) )
        IF (KDIR == NH2D) THEN
#ifdef WITH_GPU_OFFLOAD
          IF(PRESENT(QUEUE))THEN
           
          ELSE
           
          ENDIF
#else
          DEV (:, :, :, :, :) = HST (:, :, :, :, :)
#endif
        ELSEIF (KDIR == ND2H) THEN
#ifdef WITH_GPU_OFFLOAD
          IF(PRESENT(QUEUE))THEN
           
          ELSE
           
          ENDIF
#else
          HST (:, :, :, :, :) = DEV (:, :, :, :, :)
#endif
        ENDIF
    END SUBROUTINE





  INTEGER (KIND=JPIM) FUNCTION FIELD_5RD_GET_LAST_CONTIGUOUS_DIMENSION (PTR, AFTER) RESULT (JDIM)
  REAL(KIND=JPRD), POINTER :: PTR (:,:,:,:,:)
  INTEGER (KIND=JPIM) :: AFTER
  INTEGER*8 :: IPREVIOUS_STRIDE, ITHIS_STRIDE, ISIZE
  INTEGER (KIND=JPIM) :: J, LB(5)

  ! assume that dimension all dimensions before AFTER are contiguous...
  LB = LBOUND(PTR)
  IF (AFTER == 0) THEN
    IPREVIOUS_STRIDE = KIND (PTR)
  ENDIF

  IF (AFTER < 1) THEN
    ISIZE = 1
    IF (SIZE(PTR, 1) /= 1) THEN
      ITHIS_STRIDE = LOC (PTR (LB(1)+1, LB(2), LB(3), LB(4), LB(5))) - LOC (PTR (LB(1), LB(2), LB(3), LB(4), LB(5)))
      IF (IPREVIOUS_STRIDE * ISIZE /= ITHIS_STRIDE) THEN
        JDIM = 0
        RETURN
      ENDIF
    ENDIF
    IPREVIOUS_STRIDE = IPREVIOUS_STRIDE * ISIZE
  ELSE IF (AFTER == 1) THEN
    ITHIS_STRIDE = LOC (PTR (LB(1)+1, LB(2), LB(3), LB(4), LB(5))) - LOC (PTR (LB(1), LB(2), LB(3), LB(4), LB(5)))
    IPREVIOUS_STRIDE = ITHIS_STRIDE
  ENDIF

  IF (AFTER < 2) THEN
    ISIZE = SIZE(PTR, 1)
    IF (SIZE(PTR, 2) /= 1) THEN
      ITHIS_STRIDE = LOC (PTR (LB(1), LB(2)+1, LB(3), LB(4), LB(5))) - LOC (PTR (LB(1), LB(2), LB(3), LB(4), LB(5)))
      IF (IPREVIOUS_STRIDE * ISIZE /= ITHIS_STRIDE) THEN
        JDIM = 1
        RETURN
      ENDIF
    ENDIF
    IPREVIOUS_STRIDE = IPREVIOUS_STRIDE * ISIZE
  ELSE IF (AFTER == 2) THEN
    ITHIS_STRIDE = LOC (PTR (LB(1), LB(2)+1, LB(3), LB(4), LB(5))) - LOC (PTR (LB(1), LB(2), LB(3), LB(4), LB(5)))
    IPREVIOUS_STRIDE = ITHIS_STRIDE
  ENDIF

  IF (AFTER < 3) THEN
    ISIZE = SIZE(PTR, 2)
    IF (SIZE(PTR, 3) /= 1) THEN
      ITHIS_STRIDE = LOC (PTR (LB(1), LB(2), LB(3)+1, LB(4), LB(5))) - LOC (PTR (LB(1), LB(2), LB(3), LB(4), LB(5)))
      IF (IPREVIOUS_STRIDE * ISIZE /= ITHIS_STRIDE) THEN
        JDIM = 2
        RETURN
      ENDIF
    ENDIF
    IPREVIOUS_STRIDE = IPREVIOUS_STRIDE * ISIZE
  ELSE IF (AFTER == 3) THEN
    ITHIS_STRIDE = LOC (PTR (LB(1), LB(2), LB(3)+1, LB(4), LB(5))) - LOC (PTR (LB(1), LB(2), LB(3), LB(4), LB(5)))
    IPREVIOUS_STRIDE = ITHIS_STRIDE
  ENDIF

  IF (AFTER < 4) THEN
    ISIZE = SIZE(PTR, 3)
    IF (SIZE(PTR, 4) /= 1) THEN
      ITHIS_STRIDE = LOC (PTR (LB(1), LB(2), LB(3), LB(4)+1, LB(5))) - LOC (PTR (LB(1), LB(2), LB(3), LB(4), LB(5)))
      IF (IPREVIOUS_STRIDE * ISIZE /= ITHIS_STRIDE) THEN
        JDIM = 3
        RETURN
      ENDIF
    ENDIF
    IPREVIOUS_STRIDE = IPREVIOUS_STRIDE * ISIZE
  ELSE IF (AFTER == 4) THEN
    ITHIS_STRIDE = LOC (PTR (LB(1), LB(2), LB(3), LB(4)+1, LB(5))) - LOC (PTR (LB(1), LB(2), LB(3), LB(4), LB(5)))
    IPREVIOUS_STRIDE = ITHIS_STRIDE
  ENDIF

  IF (AFTER < 5) THEN
    ISIZE = SIZE(PTR, 4)
    IF (SIZE(PTR, 5) /= 1) THEN
      ITHIS_STRIDE = LOC (PTR (LB(1), LB(2), LB(3), LB(4), LB(5)+1)) - LOC (PTR (LB(1), LB(2), LB(3), LB(4), LB(5)))
      IF (IPREVIOUS_STRIDE * ISIZE /= ITHIS_STRIDE) THEN
        JDIM = 4
        RETURN
      ENDIF
    ENDIF
    IPREVIOUS_STRIDE = IPREVIOUS_STRIDE * ISIZE
  ELSE IF (AFTER == 5) THEN
    ITHIS_STRIDE = LOC (PTR (LB(1), LB(2), LB(3), LB(4), LB(5)+1)) - LOC (PTR (LB(1), LB(2), LB(3), LB(4), LB(5)))
    IPREVIOUS_STRIDE = ITHIS_STRIDE
  ENDIF

  JDIM = 5
  END FUNCTION FIELD_5RD_GET_LAST_CONTIGUOUS_DIMENSION


END MODULE FIELD_5RD_DATA_MODULE
