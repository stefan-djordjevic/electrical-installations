!--------------------------------------------------------------------------------------
!FORMULE POTREBNE ZA PRORACUN PRESEKA KABLOVA

SUBROUTINE FORMULA_ZA_Pj_ZGRADE(k, n, Pjs, Pjz)

    IMPLICIT NONE

    REAL,    INTENT(IN)  :: k
    REAL,    INTENT(IN)  :: n
    REAL,    INTENT(IN)  :: Pjs
    REAL,    INTENT(OUT) :: Pjz

    Pjz = ( k + (1-k)/SQRT(n) ) * n * Pjs

END SUBROUTINE FORMULA_ZA_Pj_ZGRADE

!--------------------------------------------------------------------------------------

SUBROUTINE FORMULA_ZA_Ib_MONOFAZNO(P, U, I_b)

    IMPLICIT NONE

    REAL,    INTENT(IN)  :: P
    REAL,    INTENT(IN)  :: U
    REAL,    INTENT(OUT) :: I_b

    I_b = P / U

END SUBROUTINE FORMULA_ZA_Ib_MONOFAZNO

!--------------------------------------------------------------------------------------

SUBROUTINE FORMULA_ZA_Ib_TROFAZNO(P, U, cos_fi, I_b)

    IMPLICIT NONE

    REAL,    INTENT(IN)  :: P
    REAL,    INTENT(IN)  :: U
    REAL,    INTENT(IN)  :: cos_fi
    REAL,    INTENT(OUT) :: I_b

    I_b = P / (SQRT(3.) * U * cos_fi)

END SUBROUTINE FORMULA_ZA_Ib_TROFAZNO

!--------------------------------------------------------------------------------------
!FORMULE ZA PRORACUN PADA NAPONA

SUBROUTINE FORMULA_ZA_Pad_Napona_MONOFAZNO(l, r, P, U, pad_U)

    IMPLICIT NONE

    REAL,    INTENT(IN)  :: l
    REAL,    INTENT(IN)  :: r
    REAL,    INTENT(IN)  :: P
    REAL,    INTENT(IN)  :: U
    REAL,    INTENT(OUT) :: pad_U

    pad_U = (( 2 * l * r * P ) / U**2) * 100

END SUBROUTINE FORMULA_ZA_Pad_Napona_MONOFAZNO

!--------------------------------------------------------------------------------------

SUBROUTINE FORMULA_ZA_Pad_Napona_TROFAZNO(l, r, P, U, pad_U)

    IMPLICIT NONE

    REAL,    INTENT(IN)  :: l
    REAL,    INTENT(IN)  :: r
    REAL,    INTENT(IN)  :: P
    REAL,    INTENT(IN)  :: U
    REAL,    INTENT(OUT) :: pad_U

    pad_U = (( l * r * P ) / U**2) * 100

END SUBROUTINE FORMULA_ZA_Pad_Napona_TROFAZNO

!--------------------------------------------------------------------------------------
!FORMULE ZA PRORACUN KRATKOG SPOJA (sistem zastite TN-C-S)

SUBROUTINE FORMULA_ZA_Iks(U, Ruk, Xuk, Iks)

    IMPLICIT NONE

    REAL,    INTENT(IN)  :: U
    REAL,    INTENT(IN)  :: Ruk
    REAL,    INTENT(IN)  :: Xuk
    REAL,    INTENT(OUT) :: Iks

    Iks = U / SQRT( (2*Ruk)**2 + (2*Xuk)**2 )

END SUBROUTINE FORMULA_ZA_Iks

!--------------------------------------------------------------------------------------