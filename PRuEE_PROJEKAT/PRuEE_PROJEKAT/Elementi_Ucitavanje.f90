MODULE M_Elementi_Ucitavanje

USE M_Elementi


IMPLICIT NONE


INTEGER :: Alloc_Error
LOGICAL :: Global_Error
INTEGER :: i, j


CLASS(Broj_Elemenata), POINTER :: p_BrE      => NULL()    
CLASS(ELEMENT), POINTER        :: p_Element  => NULL()
CLASS(VOD), POINTER            :: p_Vod      => NULL()
CLASS(STRUJNI_KRUG), POINTER   :: p_Str_krug => NULL()
CLASS(STAN), POINTER           :: p_Stan     => NULL()


TYPE(VOD), ALLOCATABLE, TARGET :: vodovi(:)
TYPE(VOD), ALLOCATABLE, TARGET :: deonice_GRO_RT(:)
TYPE(STRUJNI_KRUG), ALLOCATABLE, TARGET :: krugovi(:)
TYPE(STAN), ALLOCATABLE, TARGET :: stanovi(:)


!-----------------------------------------------------------------------------------------

CONTAINS


    FUNCTION Ucitaj_Broj_Elemenata() RESULT(Error)

        LOGICAL :: Error
        Error = .false.

        !Kreiranje objekta tipa Broj_Elemenata
        ALLOCATE(p_BrE, STAT = Alloc_Error)

        IF (Alloc_Error .NE. 0) THEN
            Error = .true.
            STOP ("[ERROR] Problem sa alokacijom p_BrE!")
        END IF

        !Ucitavanaje podataka
        Global_Error = p_BrE%Ucitaj_Br_El()

    END FUNCTION Ucitaj_Broj_Elemenata

    !-----------------------------------------------------------------------------------------
    
    FUNCTION Ucitaj_Vodove() RESULT(Error)
        
        LOGICAL :: Error
        Error = .false.

        
        IF ( .NOT.ALLOCATED(vodovi) ) ALLOCATE ( vodovi(p_BrE%Get_Broj_Vodova()), STAT = Alloc_Error )
        IF (Alloc_Error .NE. 0) THEN
            Error = .true.
            STOP ("[ERROR] Problem sa alokacijom memorije za vodove!")
        END IF
        
        
        OPEN (ID_File, FILE='ULAZ_Podaci_Vodovi.txt', ACTION = 'READ', STATUS = 'OLD', ERR = 100)
       
        DO i = 1, p_BrE%Get_Broj_Vodova()
            p_Element => vodovi(i)
            Global_Error = p_Element%Ucitaj()
        ENDDO                 

        CLOSE(ID_File)


        RETURN

        100 WRITE (*,*)  "[ERROR] Ne postoji ulazna datoteka 'ULAZ_Podaci_Vodovi.txt' na odgovarajucoj lokaciji!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_Vodove
    
    !-----------------------------------------------------------------------------------------

    FUNCTION Ucitaj_deonice_GRO_RT() RESULT(Error)
        
        LOGICAL :: Error
        Error = .false.

        
        IF ( .NOT.ALLOCATED(deonice_GRO_RT) ) ALLOCATE ( deonice_GRO_RT(p_BrE%Get_Broj_Stanova()), STAT = Alloc_Error )
        IF (Alloc_Error .NE. 0) THEN
            Error = .true.
            STOP ("[ERROR] Problem sa alokacijom memorije za deonice GRO-RT!")
        END IF
        
        
        OPEN (ID_File, FILE='ULAZ_Podaci_GRO_RT.txt', ACTION = 'READ', STATUS = 'OLD', ERR = 100)
       
        DO i = 1, p_BrE%Get_Broj_Stanova()
            p_Vod => deonice_GRO_RT(i)
            Global_Error = p_Vod%Ucitaj_GRO_RT()
        ENDDO                 

        CLOSE(ID_File)


        RETURN

        100 WRITE (*,*)  "[ERROR] Ne postoji ulazna datoteka 'ULAZ_Podaci_GRO_RT.txt' na odgovarajucoj lokaciji!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_deonice_GRO_RT

    !-----------------------------------------------------------------------------------------

    FUNCTION Ucitaj_strujne_krugove() RESULT(Error)
        
        LOGICAL :: Error
        Error = .false.

        
        IF ( .NOT.ALLOCATED(krugovi) ) ALLOCATE ( krugovi(p_BrE%Get_Broj_Str_Krugova()), STAT = Alloc_Error )
        IF (Alloc_Error .NE. 0) THEN
            Error = .true.
            STOP ("[ERROR] Problem sa alokacijom memorije za strujne krugove!")
        END IF
        
        
        OPEN (ID_File, FILE='ULAZ_Podaci_Strujni_Krugovi.txt', ACTION = 'READ', STATUS = 'OLD', ERR = 100)
       
        UCITAVANJE_STR_KRUGOVA: DO i = 1, p_BrE%Get_Broj_Str_Krugova()
            p_Vod => krugovi(i)
            Global_Error = p_Vod%Ucitaj()
        ENDDO UCITAVANJE_STR_KRUGOVA               

        CLOSE(ID_File)


        RETURN

        100 WRITE (*,*)  "[ERROR] Ne postoji ulazna datoteka 'ULAZ_Podaci_Strujni_Krugovi.txt' na odgovarajucoj lokaciji!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_strujne_krugove

    !-----------------------------------------------------------------------------------------

    FUNCTION Ucitaj_stanove() RESULT(Error)
        
        LOGICAL :: Error
        Error = .false.

        
        IF ( .NOT.ALLOCATED(stanovi) ) ALLOCATE ( stanovi(p_BrE%Get_Broj_Stanova()), STAT = Alloc_Error )
        IF (Alloc_Error .NE. 0) THEN
            Error = .true.
            STOP ("[ERROR] Problem sa alokacijom memorije za stanove!")
        END IF
        
        
        OPEN (ID_File, FILE='ULAZ_Podaci_Stan.txt', ACTION = 'READ', STATUS = 'OLD', ERR = 100)
       
        PRESKOCI_RED: DO i=1, 2
            READ (ID_File, *)
        ENDDO PRESKOCI_RED

        DO i = 1, p_BrE%Get_Broj_Stanova()
            p_Element => stanovi(i)
            Global_Error = p_Element%Ucitaj()
        ENDDO              

        CLOSE(ID_File)


        RETURN

        100 WRITE (*,*)  "[ERROR] Ne postoji ulazna datoteka 'ULAZ_Podaci_Stan.txt' na odgovarajucoj lokaciji!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_stanove

    !-----------------------------------------------------------------------------------------


END MODULE M_Elementi_Ucitavanje 