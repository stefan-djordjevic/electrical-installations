MODULE M_Globalni_Proracun

USE M_Elementi
USE M_Elementi_Ucitavanje


IMPLICIT NONE


INCLUDE "Formule_za_proracun.fi"


INTEGER, PARAMETER :: MONOFAZNI = 1, TROFAZNI = 3


REAL, PARAMETER :: k_jednovremeno = 0.25 !koeficijent jednovremenosti za beskonacan broj stanova

REAL :: Pj_1_zgrada, Pj_2_zgrade         !jednovremena snaga jedne zgrade i jednovremena snaga dve zgrade, respektivno
REAL :: Pj_stan_avrg = 0                 !prosecna jednovremena snaga stanova
REAL :: Pj_stan_uk = 0                   !ukupna jednovremena snaga stanova



CONTAINS

    !----------------------------------------------------------------------------------------

    SUBROUTINE Proracun_P_jednovremeno_STAN()

        !Proracun instalisane snage za svaki stan
        DO i=1, p_BrE%Get_Broj_Stanova() 
            DO j=1, p_BrE%Get_Broj_Str_Krugova()
                
                IF( krugovi(j)%Get_Sifra_Stana() .eq. stanovi(i)%Get_Sifra() ) THEN

                    stanovi(i)%P_inst_stan = stanovi(i)%P_inst_stan + krugovi(j)%Get_P_inst_str_krug()

                ENDIF
            
            ENDDO
        ENDDO

        !Proracun jednovremene snage za svaki stan
        DO i=1, p_BrE%Get_Broj_Stanova()

            p_Stan => stanovi(i)
            p_Stan%P_jednovremeno = p_Stan%Get_k_potraznje() * p_Stan%P_inst_stan

            Pj_stan_uk = Pj_stan_uk + p_Stan%P_jednovremeno

        ENDDO

        !Proracun prosecne jednovremene snage stanova
        Pj_stan_avrg = Pj_stan_uk / SIZE(stanovi)


    END SUBROUTINE Proracun_P_jednovremeno_STAN

    !----------------------------------------------------------------------------------------
    
    SUBROUTINE Proracun_P_jednovremeno_ZGRADA()

        !Proracun Pj jedne zgrade
        CALL FORMULA_ZA_Pj_ZGRADE(k_jednovremeno, REAL(p_BrE%Get_Broj_Stanova() + 1), Pj_stan_avrg, Pj_1_zgrada)

        !Proracun Pj dve zgrade
        CALL FORMULA_ZA_Pj_ZGRADE(k_jednovremeno, REAL(2 * p_BrE%Get_Broj_Stanova() + 2), Pj_stan_avrg, Pj_2_zgrade)

    END SUBROUTINE Proracun_P_jednovremeno_ZGRADA

    !----------------------------------------------------------------------------------------


END MODULE M_Globalni_Proracun