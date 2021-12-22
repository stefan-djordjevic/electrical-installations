MODULE M_Pad_Napona_i_Kratak_Spoj

USE M_Elementi
USE M_Elementi_Ucitavanje
USE M_Globalni_Proracun


IMPLICIT NONE


REAL :: pad_Napona_TS_KPK_N          !ukupan pad napona na napojnom vodu od TS do najudaljenije zgrade (KPK_N)
REAL :: pad_Napona_TS_KPK_N_min_1    !pad napona na napojnom kablu od TS do kompleksa od 2 zgrade (KPK_N-1)

REAL :: duzina_TS_KPK_N_min_1        !duzina napojnog kabla od TS do kompleksa od 2 zgrade (KPK_N-1)

REAL :: R_TS_KPK_N_min_1             !rezistansa napojnog kabla od TS do kompleksa od 2 zgrade (KPK_N-1)
REAL :: X_TS_KPK_N_min_1             !reaktansa napojnog kabla od TS do kompleksa od 2 zgrade (KPK_N-1)

REAL :: R_TS_GRO                     !ukupna rezistansa na napojnom vodu od TS do GRO najudaljenije zgrade
REAL :: X_TS_GRO                     !ukupna reaktansa na napojnom vodu od TS do GRO najudaljenije zgrade

REAL :: Kratak_Spoj_GRO              !Kratak spoj na mestu GRO najudaljenije zgrade 


CONTAINS


    !------------------------------------------------------------------------------------------
    !Proracun pada napona na VODOVIMA i kratkog spoja na mestu GRO najudaljenije zgrade

    SUBROUTINE Proracun_Pada_Napona_Vodova_i_KS_GRO()
    

        DO i=1, SIZE(vodovi)
            
            p_Vod => vodovi(i)

            !Parametri R i X
            p_Vod%R = p_Vod%Get_Duzina() * p_Vod%r_poduzno
            p_Vod%X = p_Vod%Get_Duzina() * p_Vod%x_poduzno

            IF (i == 1) THEN
                !pad napona na napojnom vodu TS-KPK1
                CALL FORMULA_ZA_Pad_Napona_TROFAZNO(p_Vod%Get_Duzina(), p_Vod%r_poduzno, Pj_2_zgrade, p_Vod%Get_Napon(), p_Vod%pad_napona)
            ELSE
                !pad napona na vodovima: KPK1-KPK2, KPK-GRO
                CALL FORMULA_ZA_Pad_Napona_TROFAZNO(p_Vod%Get_Duzina(), p_Vod%r_poduzno, Pj_1_zgrada, p_Vod%Get_Napon(), p_Vod%pad_napona)
            ENDIF
        
        ENDDO
        
        
        duzina_TS_KPK_N_min_1 = vodovi(1)%Get_Duzina() + ((p_BrE%Get_Broj_Zgrada() - 2) * (vodovi(2)%Get_Duzina() - 0.005))

        R_TS_KPK_N_min_1 = duzina_TS_KPK_N_min_1 * vodovi(1)%r_poduzno
        X_TS_KPK_N_min_1 = duzina_TS_KPK_N_min_1 * vodovi(1)%x_poduzno

        R_TS_GRO = R_TS_KPK_N_min_1 + vodovi(2)%R + vodovi(3)%R
        X_TS_GRO = X_TS_KPK_N_min_1 + vodovi(2)%X + vodovi(3)%X
  
        !pad napona: TS-KPK_N-1
        CALL FORMULA_ZA_Pad_Napona_TROFAZNO(duzina_TS_KPK_N_min_1, vodovi(1)%r_poduzno, Pj_2_zgrade, vodovi(1)%Get_Napon(), pad_Napona_TS_KPK_N_min_1)

        !pad napona: TS-KPK_N = TS-KPK_N-1 + KPK1-KPK2
        pad_Napona_TS_KPK_N = pad_Napona_TS_KPK_N_min_1 + vodovi(2)%pad_napona

        !Kratak spoj na mestu GRO najudaljenije zgrade
        CALL FORMULA_ZA_Iks( vodovi(3)%Get_Napon()/SQRT(3.), R_TS_GRO, X_TS_GRO, Kratak_Spoj_GRO )


    END SUBROUTINE Proracun_Pada_Napona_Vodova_i_KS_GRO



    !--------------------------------------------------------------------------------------
    !Proracun pada napona na STRUJNIM KRUGOVIMA

    SUBROUTINE Proracun_Pada_Napona_Strujnih_Krugova()

        DO i = 1, p_BrE%Get_Broj_Str_Krugova()
            
            p_Str_krug => krugovi(i)

            !Parametri R i X
            p_Str_krug%R = p_Str_krug%Get_Duzina() * p_Str_krug%r_poduzno
            p_Str_krug%X = p_Str_krug%Get_Duzina() * p_Str_krug%x_poduzno

            SELECT CASE (p_Str_krug%Get_Broj_Faza())
                
                !Pad napona na monofaznim strujnim krugovima
                CASE (MONOFAZNI)
                    CALL FORMULA_ZA_Pad_Napona_MONOFAZNO(p_Str_krug%Get_Duzina(), p_Str_krug%r_poduzno, p_Str_krug%Get_P_inst_str_krug(), p_Str_krug%Get_Napon(), p_Str_krug%pad_napona)
                
                !Pad napona na trofaznim strujnim krugovima
                CASE (TROFAZNI)
                    CALL FORMULA_ZA_Pad_Napona_TROFAZNO(p_Str_krug%Get_Duzina(), p_Str_krug%r_poduzno, p_Str_krug%Get_P_inst_str_krug(), p_Str_krug%Get_Napon(), p_Str_krug%pad_napona)

            END SELECT          
                           
        ENDDO
        
        
    END SUBROUTINE Proracun_Pada_Napona_Strujnih_Krugova



    !--------------------------------------------------------------------------------------
    !Proracun pada napona na DEONICAMA GRO-RT, kao i ukupnog pada napona na deonici od TS do potrosaca najudaljenije zgrade
    !i kratkog spoja na potrosacima stanova najudaljenije zgrade

    SUBROUTINE Proracun_Pada_Napona_GRO_RT_i_TS_Potr_i_KS_Potr()

        REAL :: R_uk
        REAL :: X_uk
        
        DO i = 1, p_BrE%Get_Broj_Stanova()
            
            p_Vod => deonice_GRO_RT(i)
            p_Stan => stanovi(i)

            !Parametri R i X
            p_Vod%R = p_Vod%Get_Duzina() * p_Vod%r_poduzno
            p_Vod%X = p_Vod%Get_Duzina() * p_Vod%x_poduzno
            
            !pad napona na DEONICI GRO-RT
            CALL FORMULA_ZA_Pad_Napona_TROFAZNO(p_Vod%Get_Duzina(), p_Vod%r_poduzno, p_Stan%P_jednovremeno, p_Vod%Get_Napon(), p_Vod%pad_napona)
            
            !Kratak spoj na mestu RT stanova najudaljenije zgrade
            CALL FORMULA_ZA_Iks( p_Vod%Get_Napon()/SQRT(3.), (p_Vod%R + R_TS_GRO), (p_Vod%X + X_TS_GRO), p_Vod%struja_k_s )

            DO j = 1, p_BrE%Get_Broj_Str_Krugova()
                
                p_Str_krug => krugovi(j)
                IF ( p_Str_krug%Get_Sifra_Stana() .EQ. p_Vod%Get_Sifra() ) THEN
                    
                    !ukupan pad napona: TS-KPK_N + KPK_N-GRO + GRO-RT(i) + STR_KRUG(j) 
                    p_Str_krug%pad_napona_TS_KRUG = pad_Napona_TS_KPK_N + vodovi(3)%pad_napona + p_Vod%pad_napona + p_Str_krug%pad_napona     

                    R_uk = R_TS_GRO + p_Vod%R + p_Str_krug%R
                    X_uk = X_TS_GRO + p_Vod%X + p_Str_krug%X

                    !Kratak spoj na potrosacima u stanovima najudaljenije zgrade
                    CALL FORMULA_ZA_Iks( p_Vod%Get_Napon()/SQRT(3.), R_uk, X_uk, p_Str_krug%struja_k_s )
                
                ENDIF

            ENDDO

        ENDDO  


    END SUBROUTINE Proracun_Pada_Napona_GRO_RT_i_TS_Potr_i_KS_Potr

    !--------------------------------------------------------------------------------------


END MODULE M_Pad_Napona_i_Kratak_Spoj