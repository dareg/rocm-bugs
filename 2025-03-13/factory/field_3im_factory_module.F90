MODULE FIELD_3IM_FACTORY_MODULE

IMPLICIT NONE

PRIVATE

INTERFACE FIELD_NEW
  MODULE PROCEDURE FIELD_3IM_NEW_GANG_WRAPPER
END INTERFACE

INTERFACE

MODULE SUBROUTINE FIELD_3IM_NEW_GANG_WRAPPER ()
END SUBROUTINE

END INTERFACE

PUBLIC :: FIELD_NEW

END MODULE

