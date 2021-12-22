MODULE M_PRESEK_Proracun

USE M_Elementi
USE M_Elementi_Ucitavanje
USE M_Globalni_Proracun
USE M_Tablice

IMPLICIT NONE


CONTAINS


    SUBROUTINE Proracun_Preseka_Svih_Deonica()

        
        CALL Proracun_Ib_VODOVA()

        CALL Proracun_Ib_DEONICA_GRO_RT()

        CALL Proracun_Ib_STRUJNI_KRUGOVI()


        !--------------------------------------------------------------------------------------
        !Proracun preseka i odabir zastitne opreme VODOVA

        DO i = 1, p_BrE%Get_Broj_Vodova()
            p_Vod => vodovi(i)
            CALL p_Vod%ODREDI_PRESEK(Itd_Al, preseci_Al, poduzne_rezistanse_Al, poduzne_reaktanse_Al)
        ENDDO   

        !--------------------------------------------------------------------------------------
        !Proracun preseka i odabir zastitne opreme DEONICA GRO-RT

        DO i = 1, p_BrE%Get_Broj_Stanova()
            p_Vod => deonice_GRO_RT(i)
            CALL p_Vod%ODREDI_PRESEK(Itd_Cu, preseci_Cu, poduzne_rezistanse_Cu, poduzne_reaktanse_Cu)
        ENDDO  

        !--------------------------------------------------------------------------------------
        !Proracun preseka i odabir zastitne opreme STRUJNIH KRUGOVA

        DO i = 1, p_BrE%Get_Broj_Str_Krugova()
            
            p_Str_krug => krugovi(i)

            SELECT CASE (p_Str_krug%Get_Osigurac())
                
                !Ako je kao podatak za osigurac uneta vrednost 0, proracunava se presek bez predlozene vrednosti za In
                CASE (0)
                    CALL p_Str_krug%ODREDI_PRESEK(Itd_Cu, preseci_Cu, poduzne_rezistanse_Cu, poduzne_reaktanse_Cu)
                
                !Ako je uneta predlozena vrednost In osiguraca, proracunava se presek sa tom vrednoscu In
                CASE (16)
                    CALL p_Str_krug%ODREDI_PRESEK(Itd_Cu, preseci_Cu, poduzne_rezistanse_Cu, poduzne_reaktanse_Cu, In(2))
                
                !Ako uneta predlozena vrednost In nije odgovarajuca (0 ili 16), prijavljuje gresku
                CASE DEFAULT
                    STOP ("[ERROR] Pogresan podatak za osigurac strujnog kruga!")

            END SELECT          
                           
        ENDDO


    END SUBROUTINE Proracun_Preseka_Svih_Deonica

    !-------------------------------------------------------------------------------------------------------
    !-------------------------------------------------------------------------------------------------------
    !-------------------------------------------------------------------------------------------------------

    SUBROUTINE Proracun_Ib_VODOVA()

        DO i=1, SIZE(vodovi)
            
            p_Vod => vodovi(i)

            IF (i == 1) THEN
                !Proracun Ib za napojni vod TS-KPK1
                CALL FORMULA_ZA_Ib_TROFAZNO(Pj_2_zgrade, p_Vod%Get_Napon(), p_Vod%Get_Faktor_Snage(), p_Vod%Ib)
            ELSE
                !Proracun Ib za vodove: KPK1-KPK2, KPK-GRO
                CALL FORMULA_ZA_Ib_TROFAZNO(Pj_1_zgrada, p_Vod%Get_Napon(), p_Vod%Get_Faktor_Snage(), p_Vod%Ib)
            ENDIF
        
        ENDDO

    END SUBROUTINE Proracun_Ib_VODOVA

    !----------------------------------------------------------------------------------------

    SUBROUTINE Proracun_Ib_DEONICA_GRO_RT()

        !Proracun Ib za deonice GRO-RT
        DO i=1, SIZE(deonice_GRO_RT)
            p_Vod => deonice_GRO_RT(i)      
            
            DO j=1, SIZE(stanovi)
                p_Stan => stanovi(j)
                
                IF(p_Stan%Get_Sifra() .eq. p_Vod%Get_Sifra()) THEN
                    CALL FORMULA_ZA_Ib_TROFAZNO(p_Stan%P_jednovremeno, p_Vod%Get_Napon(), p_Vod%Get_Faktor_Snage(), p_Vod%Ib)        
                ENDIF

            ENDDO
        ENDDO

    END SUBROUTINE Proracun_Ib_DEONICA_GRO_RT

    !----------------------------------------------------------------------------------------

    SUBROUTINE Proracun_Ib_STRUJNI_KRUGOVI()

        !Proracun Ib za strujne krugove u stanovima
        DO i=1, SIZE(krugovi)
            
            p_Str_krug => krugovi(i)  
                
            
            SELECT CASE (p_Str_krug%Get_Broj_Faza())
                
                CASE (MONOFAZNI)
                    CALL FORMULA_ZA_Ib_MONOFAZNO(p_Str_krug%Get_P_inst_str_krug(), p_Str_krug%Get_Napon(), p_Str_krug%Ib)
                
                CASE (TROFAZNI)
                    CALL FORMULA_ZA_Ib_TROFAZNO(p_Str_krug%Get_P_inst_str_krug(), p_Str_krug%Get_Napon(), p_Str_krug%Get_Faktor_Snage(), p_Str_krug%Ib)        
            
                CASE DEFAULT
                    STOP ("[ERROR] Pogresan podatak za broj faza strujnog kruga!")

            END SELECT


        ENDDO

    END SUBROUTINE Proracun_Ib_STRUJNI_KRUGOVI

    !----------------------------------------------------------------------------------------

END MODULE M_PRESEK_Proracun