MODULE M_Elementi

USE M_Tablice


IMPLICIT NONE


INTEGER, PARAMETER :: ID_File = 20


!------------------------------------------------------------------------------------------------
!Podaci o broju elemenata

TYPE, PUBLIC :: Broj_Elemenata

    PRIVATE
        INTEGER :: broj_Vodova          !Od TS do GRO druge zgrade
        INTEGER :: broj_Zgrada          !Ukupan broj zgrada
        INTEGER :: broj_Stanova         !Broj stanova u jednoj zgradi
        INTEGER :: broj_Str_krugova     !Ukupan broj strujnih krugova svih stanova jedne zgrade 


    CONTAINS

        PROCEDURE, PUBLIC :: Ucitaj_Br_El           => Ucitaj_podatke_o_broju_elemenata
        PROCEDURE, PUBLIC :: Get_Broj_Vodova        => Uzmi_podatak_za_broj_vodova
        PROCEDURE, PUBLIC :: Get_Broj_Zgrada        => Uzmi_podatak_za_broj_zgrada
        PROCEDURE, PUBLIC :: Get_Broj_Stanova       => Uzmi_podatak_za_broj_stanova
        PROCEDURE, PUBLIC :: Get_Broj_Str_Krugova   => Uzmi_podatak_za_broj_str_krugova


END TYPE Broj_Elemenata

PRIVATE :: Ucitaj_podatke_o_broju_elemenata, Uzmi_podatak_za_broj_vodova, Uzmi_podatak_za_broj_zgrada
PRIVATE :: Uzmi_podatak_za_broj_stanova, Uzmi_podatak_za_broj_str_krugova


!------------------------------------------------------------------------------------------------
!Korisnicki definisani tip koji modeluje element mreze sa zgradama

TYPE, PUBLIC :: ELEMENT

    PRIVATE
        INTEGER       :: redni_br         !redni broj elementa
        INTEGER       :: sifra            !sifra elementa
        CHARACTER(20) :: naziv = ' '      !naziv elementa
  

    CONTAINS
        PROCEDURE, PUBLIC :: Ucitaj             => Ucitaj_podatke_za_element
        PROCEDURE, PUBLIC :: Get_Redni_br       => Uzmi_redni_br_za_element  
        PROCEDURE, PUBLIC :: Get_Sifra          => Uzmi_sifru_za_element
        PROCEDURE, PUBLIC :: Get_Naziv          => Uzmi_naziv_za_element

END TYPE ELEMENT


PRIVATE :: Ucitaj_podatke_za_element, Uzmi_sifru_za_element, Uzmi_redni_br_za_element, Uzmi_naziv_za_element


!------------------------------------------------------------------------------------------------
!Korisnicki definisani tip koji modeluje vodove

TYPE, PUBLIC, EXTENDS(ELEMENT) :: VOD
    
    PRIVATE
        REAL    :: napon            !napon deonice    
        REAL    :: faktor_snage     !faktor snage deonice   
        REAL    :: k_n              !korekcioni faktor za grupno polozena strujna kola ili visezilnih kablova
        REAL    :: k_theta          !korekcioni faktor za okolnu temperaturu
        REAL    :: k_lambda         !korekcioni faktor za termicku otpornost tla
        REAL    :: k_osiguraca      !koeficijent reagovanja osiguraca
        REAL    :: duzina           !duzina elementa

    REAL, PUBLIC    :: Ib                   !maksimalna jednovremena struja kabla
    REAL, PUBLIC    :: In_odredjeno = 0     !naznacena struja zastitnog uredjaja
    REAL, PUBLIC    :: Iz = 0               !stvarna trajno dozvoljena struja kabla
    REAL, PUBLIC    :: presek = 0           !presek kabla [mm**2]
    REAL, PUBLIC    :: r_poduzno = 0        !poduzna rezistansa deonice [Ohm/km]
    REAL, PUBLIC    :: x_poduzno = 0        !poduzna reaktansa deonice [Ohm/km]
    REAL, PUBLIC    :: R                    !rezistansa deonice [Ohm]
    REAL, PUBLIC    :: X                    !reaktansa deonice [Ohm]
    REAL, PUBLIC    :: pad_napona = 0       !pad napona na deonici [%]
    REAL, PUBLIC    :: struja_k_s = 0       !struja kratkog spoja deonice


    CONTAINS

        PROCEDURE, PUBLIC :: Ucitaj             => Ucitaj_podatke_za_deonicu
        PROCEDURE, PUBLIC :: Ucitaj_GRO_RT      => Ucitaj_podatke_za_deonicu_GRO_RT  
        
        PROCEDURE, PUBLIC :: Get_Napon          => Uzmi_napon_deonice
        PROCEDURE, PUBLIC :: Get_Faktor_Snage   => Uzmi_faktor_snage_deonice
        PROCEDURE, PUBLIC :: Get_K_n            => Uzmi_k_n_za_deonicu
        PROCEDURE, PUBLIC :: Get_K_theta        => Uzmi_k_theta_za_deonicu
        PROCEDURE, PUBLIC :: Get_K_lambda       => Uzmi_k_lambda_za_deonicu
        PROCEDURE, PUBLIC :: Get_K_osiguraca    => Uzmi_k_osiguraca_za_deonicu
        PROCEDURE, PUBLIC :: Get_Duzina         => Uzmi_duzinu_za_deonicu

        PROCEDURE, PUBLIC :: ODREDI_PRESEK      => Proracunaj_presek_deonice           
                                                                    

END TYPE VOD

PRIVATE :: Ucitaj_podatke_za_deonicu, Uzmi_napon_deonice, Uzmi_faktor_snage_deonice
PRIVATE :: Uzmi_k_n_za_deonicu, Uzmi_k_theta_za_deonicu, Uzmi_k_lambda_za_deonicu, Uzmi_k_osiguraca_za_deonicu
PRIVATE :: Uzmi_duzinu_za_deonicu, Ucitaj_podatke_za_deonicu_GRO_RT

!------------------------------------------------------------------------------------------------
!Korisnicki definisani tip koji modeluje strujni krug u stanu

TYPE, PUBLIC, EXTENDS(VOD) :: STRUJNI_KRUG

    PRIVATE
        INTEGER :: sifra_stana               !sifra stana kome strujni krug pripada
        INTEGER :: broj_faza                 !broj faza strujnog kruga
        INTEGER :: osigurac                  !predlozena In osiguraca na strujnom krugu
        REAL    :: P_inst_str_krug           !instalisana snaga strujnog kruga    
        
    REAL, PUBLIC :: pad_napona_TS_KRUG       !jednovremena snaga deonice od GRO do RT, odnosno jedne zgrade


    CONTAINS
        
        PROCEDURE, PUBLIC :: Ucitaj                 => Ucitaj_podatke_za_strujni_krug
        PROCEDURE, PUBLIC :: Get_Sifra_Stana        => Uzmi_sifru_stana_strujnog_kruga
        PROCEDURE, PUBLIC :: Get_Broj_Faza          => Uzmi_broj_faza_strujnog_kruga
        PROCEDURE, PUBLIC :: Get_Osigurac           => Uzmi_osigurac_strujnog_kruga
        PROCEDURE, PUBLIC :: Get_P_inst_str_krug    => Uzmi_P_instalisano_strujnog_kruga


END TYPE STRUJNI_KRUG

PRIVATE :: Ucitaj_podatke_za_strujni_krug, Uzmi_broj_faza_strujnog_kruga, Uzmi_osigurac_strujnog_kruga
PRIVATE :: Uzmi_P_instalisano_strujnog_kruga, Uzmi_sifru_stana_strujnog_kruga

!------------------------------------------------------------------------------------------------
!Korisnicki definisani tip koji modeluje stan

TYPE, PUBLIC, EXTENDS(ELEMENT) :: STAN

    PRIVATE
        REAL :: k_potraznje             !koeficijent potraznje stana
                   
    REAL, PUBLIC :: P_inst_stan = 0     !instalisana snaga stana
    REAL, PUBLIC :: P_jednovremeno      !jednovremena snaga stana            !    

    CONTAINS
        
        PROCEDURE, PUBLIC :: Ucitaj                 => Ucitaj_podatke_za_stan
        PROCEDURE, PUBLIC :: Get_k_potraznje        => Uzmi_k_potraznje_stana


END TYPE STAN

PRIVATE :: Ucitaj_podatke_za_stan, Uzmi_k_potraznje_stana


CONTAINS

!------------------------------------------------------------------------------------------------
!DEFINICIJE METODA

    FUNCTION Ucitaj_podatke_o_broju_elemenata(this) RESULT (Error)


        CLASS (Broj_Elemenata) :: this
        LOGICAL :: Error

        Error = .false.

        
        OPEN (UNIT = ID_File, FILE='ULAZ_Broj_Elemenata.txt', ACTION = 'READ', STATUS = 'UNKNOWN', ERR = 100)

        
        READ (ID_File, *, ERR=101) this%broj_Vodova
        READ (ID_File, *, ERR=101) this%broj_Zgrada
        READ (ID_File, *, ERR=101) this%broj_Stanova
        READ (ID_File, *, ERR=101) this%broj_Str_krugova

        CLOSE(ID_File)


        RETURN

        100 WRITE (*,*)  "[ERROR] Ne postoji ulazna datoteka 'ULAZ_Broj_Elemenata.txt' na odgovarajucoj lokaciji!"
        Error = .true.
        STOP

        101 WRITE (*,*) "[ERROR] Greska u formatu ulazne datoteke 'ULAZ_Broj_Elemenata.txt'!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_podatke_o_broju_elemenata

!------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_podatak_za_broj_vodova(this) RESULT(broj_Vod)


        CLASS (Broj_Elemenata) :: this
        INTEGER :: broj_Vod

        Broj_Vod = this%broj_Vodova


    END FUNCTION Uzmi_podatak_za_broj_vodova

!------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_podatak_za_broj_zgrada(this) RESULT(broj_Zgr)


        CLASS (Broj_Elemenata) :: this
        INTEGER :: broj_Zgr

        broj_Zgr = this%broj_Zgrada


    END FUNCTION Uzmi_podatak_za_broj_zgrada

!------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_podatak_za_broj_stanova(this) RESULT(broj_Sta)


        CLASS (Broj_Elemenata) :: this
        INTEGER :: broj_Sta

        broj_Sta = this%broj_Stanova


    END FUNCTION Uzmi_podatak_za_broj_stanova

!------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_podatak_za_broj_str_krugova(this) RESULT(broj_Kru)


        CLASS (Broj_Elemenata) :: this
        INTEGER :: broj_Kru

        broj_Kru = this%broj_Str_krugova


    END FUNCTION Uzmi_podatak_za_broj_str_krugova

!------------------------------------------------------------------------------------------------

    FUNCTION Ucitaj_podatke_za_element(this) RESULT(Error)


        CLASS (ELEMENT)  :: this
        LOGICAL :: Error

        Error = .false.

    END FUNCTION Ucitaj_podatke_za_element

    !------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_redni_br_za_element(this) RESULT(rbr_el)


        CLASS (ELEMENT) :: this
        INTEGER :: rbr_el

        rbr_el = this%redni_br


    END FUNCTION Uzmi_redni_br_za_element

!------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_sifru_za_element(this) RESULT(sifra_el)


        CLASS (ELEMENT) :: this
        INTEGER :: sifra_el

        sifra_el = this%sifra


    END FUNCTION Uzmi_sifru_za_element
    
!------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_naziv_za_element(this) RESULT(naziv_el)


        CLASS (ELEMENT) :: this
        CHARACTER(15) :: naziv_el

        naziv_el = this%naziv


    END FUNCTION Uzmi_naziv_za_element
!------------------------------------------------------------------------------------------------

    FUNCTION Ucitaj_podatke_za_deonicu(this) RESULT(Error)

        CLASS (VOD) :: this
        LOGICAL :: Error

        Error = .false.


        READ (ID_File, *, ERR = 101) this%redni_br, this%sifra, this%naziv, this%napon, this%faktor_snage, &
                                     this%k_n, this%k_theta, this%k_lambda, this%k_osiguraca, this%duzina


        RETURN

        101 WRITE (*,*) "[ERROR] Greska u formatu ulazne datoteke 'ULAZ_Podaci_Vodovi.txt'!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_podatke_za_deonicu

!------------------------------------------------------------------------------------------------

    FUNCTION Ucitaj_podatke_za_deonicu_GRO_RT(this) RESULT(Error)

        CLASS (VOD) :: this
        LOGICAL :: Error

        Error = .false.


        READ (ID_File, *, ERR = 101) this%redni_br, this%sifra, this%naziv, this%napon, this%faktor_snage, &
                                     this%k_n, this%k_theta, this%k_lambda, this%k_osiguraca, this%duzina


        RETURN

        101 WRITE (*,*) "[ERROR] Greska u formatu ulazne datoteke 'ULAZ_Podaci_GRO_RT.txt'!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_podatke_za_deonicu_GRO_RT

!------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_napon_deonice(this) RESULT(nap)


        CLASS (VOD) :: this
        REAL :: nap

        nap = this%napon


    END FUNCTION Uzmi_napon_deonice

!------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_faktor_snage_deonice(this) RESULT(faktor)


        CLASS (VOD) :: this
        REAL :: faktor

        faktor = this%faktor_snage


    END FUNCTION Uzmi_faktor_snage_deonice

!------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_k_n_za_deonicu(this) RESULT(kn)


        CLASS (VOD) :: this
        REAL :: kn

        kn = this%k_n


    END FUNCTION Uzmi_k_n_za_deonicu

!------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_k_theta_za_deonicu(this) RESULT(ktheta)


        CLASS (VOD) :: this
        REAL :: ktheta

        ktheta = this%k_theta


    END FUNCTION Uzmi_k_theta_za_deonicu

!------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_k_lambda_za_deonicu(this) RESULT(klambda)


        CLASS (VOD) :: this
        REAL :: klambda

        klambda = this%k_lambda


    END FUNCTION Uzmi_k_lambda_za_deonicu

!------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_k_osiguraca_za_deonicu(this) RESULT(kosiguraca)


        CLASS (VOD) :: this
        REAL :: kosiguraca

        kosiguraca = this%k_osiguraca


    END FUNCTION Uzmi_k_osiguraca_za_deonicu

!------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_duzinu_za_deonicu(this) RESULT(duz)


        CLASS (VOD) :: this
        REAL :: duz

        duz = this%duzina


    END FUNCTION Uzmi_duzinu_za_deonicu

!------------------------------------------------------------------------------------------------

    FUNCTION Ucitaj_podatke_za_strujni_krug(this) RESULT(Error)

        CLASS (STRUJNI_KRUG) :: this
        LOGICAL :: Error

        Error = .false.


        READ (ID_File, *, ERR = 101) this%redni_br, this%sifra_stana, this%sifra, this%naziv, this%napon, this%faktor_snage, &
                                     this%k_n, this%k_theta, this%k_lambda, this%k_osiguraca, this%duzina, &
                                     this%broj_faza, this%osigurac, this%P_inst_str_krug


        RETURN

        101 WRITE (*,*) "[ERROR] Greska u formatu ulazne datoteke 'ULAZ_Podaci_Strujni_Krugovi.txt'!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_podatke_za_strujni_krug

!------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_sifru_stana_strujnog_kruga(this) RESULT(sifra_st)


        CLASS (STRUJNI_KRUG) :: this
        INTEGER :: sifra_st

        sifra_st = this%sifra_stana


    END FUNCTION Uzmi_sifru_stana_strujnog_kruga

!------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_broj_faza_strujnog_kruga(this) RESULT(broj_f)


        CLASS (STRUJNI_KRUG) :: this
        INTEGER :: broj_f

        broj_f = this%broj_faza


    END FUNCTION Uzmi_broj_faza_strujnog_kruga

!------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_osigurac_strujnog_kruga(this) RESULT(osigur)


        CLASS (STRUJNI_KRUG) :: this
        INTEGER :: osigur

        osigur = this%osigurac


    END FUNCTION Uzmi_osigurac_strujnog_kruga

!------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_P_instalisano_strujnog_kruga(this) RESULT(P_inst_str_k)


        CLASS (STRUJNI_KRUG) :: this
        REAL :: P_inst_str_k

        P_inst_str_k = this%P_inst_str_krug


    END FUNCTION Uzmi_P_instalisano_strujnog_kruga

!------------------------------------------------------------------------------------------------

    FUNCTION Ucitaj_podatke_za_stan(this) RESULT(Error)

        CLASS (STAN) :: this
        LOGICAL :: Error

        Error = .false.


        READ (ID_File, *, ERR = 101) this%redni_br, this%sifra, this%naziv, this%k_potraznje


        RETURN

        101 WRITE (*,*) "[ERROR] Greska u formatu ulazne datoteke 'ULAZ_Podaci_Stan.txt'!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_podatke_za_stan

!------------------------------------------------------------------------------------------------

    FUNCTION Uzmi_k_potraznje_stana(this) RESULT(k_potr)


        CLASS (STAN) :: this
        REAL :: k_potr

        k_potr = this%k_potraznje


    END FUNCTION Uzmi_k_potraznje_stana

!------------------------------------------------------------------------------------------------

    SUBROUTINE Proracunaj_presek_deonice(this, Itd_provodnika, preseci_provodnika, poduzne_rezistanse_provodnika, poduzne_reaktanse_provodnika, osigurac_s_k)

        CLASS(VOD) :: this
        REAL, INTENT(IN) :: Itd_provodnika(:)
        REAL, INTENT(IN) :: preseci_provodnika(:)
        REAL, INTENT(IN) :: poduzne_rezistanse_provodnika(:)
        REAL, INTENT(IN) :: poduzne_reaktanse_provodnika(:)
        REAL, INTENT(IN), OPTIONAL :: osigurac_s_k

        INTEGER :: i
        REAL :: I2, k_k_k
        
        LOGICAL :: Prekidac_Error = .true.
        LOGICAL :: Presek_Error = .true.

        
        !Odredi In osiguraca za datu deonicu
        IF ( PRESENT(osigurac_s_k) ) THEN
            
            this%In_odredjeno = osigurac_s_k
            Prekidac_Error = .false.
        
        ELSE    
            
            DO i=1, SIZE(In)
                
                !Uslov za izbor In osiguraca
                IF ( In(i) >= (1.1 * this%Ib) ) THEN

                    this%In_odredjeno = In(i) 
                    Prekidac_Error = .false.
                    EXIT

                ELSE
                    CYCLE                   
                
                ENDIF
            
            ENDDO

        ENDIF


        !Odredi Itd tablicno za datu deonicu, kao i presek i poduznu rezistansu (AKO POSTOJI OSIGURAC ZA DATU STRUJU Ib DEONICE!)

        IF (this%In_odredjeno .NE. 0) THEN

            I2 = this%Get_K_osiguraca() * this%In_odredjeno
            k_k_k = this%Get_K_n() * this%Get_K_theta() * this%Get_K_lambda()


            DO i=1, SIZE(Itd_provodnika)
                    
                !Uslov za izbor preseka provodnika
                IF ( (this%In_odredjeno <= k_k_k * Itd_provodnika(i)) .AND. (I2 <= 1.45 * k_k_k * Itd_provodnika(i)) ) THEN
                        
                    this%Iz = k_k_k * Itd_provodnika(i)
                    this%presek = preseci_provodnika(i)
                    this%r_poduzno = poduzne_rezistanse_provodnika(i)
                    this%x_poduzno = poduzne_reaktanse_provodnika(i)
                    Presek_Error = .false.
                    EXIT
                    
                ELSE
                    CYCLE
                ENDIF

            ENDDO
        ENDIF
                
        IF (Prekidac_Error) WRITE(*,'(a49, 1x, i4, 2x, a15)') '[WARNING] Greska pri izboru osiguraca za deonicu:', this%Get_Sifra(), this%Get_Naziv()
        IF (Presek_Error) WRITE(*,'(a47, 1x, i4, 2x, a15)') '[WARNING] Greska pri izboru preseka za deonicu:', this%Get_Sifra(), this%Get_Naziv()

    END SUBROUTINE Proracunaj_presek_deonice



END MODULE M_Elementi