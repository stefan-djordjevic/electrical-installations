SUBROUTINE Ispis_Dealokacija()

USE M_Elementi
USE M_Elementi_Ucitavanje
USE M_Pad_Napona_i_Kratak_Spoj


IMPLICIT NONE

    
    INTEGER :: Dealloc_Error
    LOGICAL :: Pad_U_Error = .true.

    !---------------------------------------------------------------------------------------------
    !Ispis

    WRITE (*, *)
    WRITE (*, *) "Ispis rezultata u izlaznu datoteku."

    OPEN (UNIT = ID_File, FILE = 'IZLAZ_Rezultati.txt', ACTION = 'WRITE', STATUS = 'UNKNOWN')
    
    
     !Ispis vodova

     WRITE (ID_File, '(/, 1x, 66("=") )' )
     WRITE (ID_File, '(24x, a)') "PRORACUN - VODOVI"
     WRITE (ID_File, '(1x, 66("=") )' ) 

     WRITE (ID_File, '(/, 1x, 66("-") )' )
     WRITE (ID_File, 301) "Sifra", "Naziv", "P[kW]", "Ib[A]", "Presek[mm2]", "Osigurac[A]"
     WRITE (ID_File, '(1x, 66("-") )' )

     DO i = 1, p_BrE%Get_Broj_Vodova()
        p_Vod => vodovi(i)
        IF(i .EQ. 1) THEN
            WRITE (ID_File, 302) p_Vod%Get_Sifra(), p_Vod%Get_Naziv(), Pj_2_zgrade/1000, p_Vod%Ib, &
                                                          INT(p_Vod%presek), INT(p_Vod%In_odredjeno)
        ELSE
            WRITE (ID_File, 302) p_Vod%Get_Sifra(), p_Vod%Get_Naziv(), Pj_1_zgrada/1000, p_Vod%Ib, &
                                                          INT(p_Vod%presek), INT(p_Vod%In_odredjeno)
        ENDIF
     ENDDO

     WRITE (ID_File, '(1x, 66("-") )' )
     WRITE (ID_File, '(1x, a)') "#aluminijumski kabl XPE  #osigurac tipa gG"

     WRITE (ID_File, '(/, 5x, 24("-"), 8x, 26("-") )' )
     WRITE (ID_File, 303) "Vod", "Pad napona[%]", "Vod", "Iks[kA]"
     WRITE (ID_File, '(5x, 24("-"), 8x, 26("-") )' )

     WRITE (ID_File, 304 ) "TS-KPK", p_BrE%Get_Broj_Zgrada(), pad_Napona_TS_KPK_N, "TS-GRO", &
                                                 p_BrE%Get_Broj_Zgrada(), Kratak_Spoj_GRO/1000

     WRITE (ID_File, 309 ) "KPK-GRO", vodovi(3)%pad_napona

     301 FORMAT (1x, a, 5x, a, 6x, a, 5x, a, 3x, a, 3x, a)
     302 FORMAT (1x, i4, 4x, a9, 3x, f7.3, 3x, f6.2, 7x, i3, 11x, i3)
     303 FORMAT (7x, a, 4x, a, 15x, a, 7x, a )
     304 FORMAT (5x, a6, i2, 4x, f5.2, 18x, a6, i2, 4x, f5.2)
     309 FORMAT (5x, a7, 5x, f5.2)



     !Ispis deonica GRO-RT

     WRITE (ID_File, '(//, 1x, 81("=") )' )
     WRITE (ID_File, '(28x, a)') "PRORACUN - DEONICE GRO-RT"
     WRITE (ID_File, '(1x, 81("=") )' ) 

     WRITE (ID_File, '(/, 1x, 81("-") )' )
     WRITE (ID_File, 305) "Sifra", "Naziv", "P[kW]", "Ib[A]", "Presek[mm2]", "Osigurac[A]", "Pad napona[%]", "Iks[A]*"
     WRITE (ID_File, '(1x, 81("-") )' )

     DO i = 1, p_BrE%Get_Broj_Stanova()
        p_Vod => deonice_GRO_RT(i)
        p_Stan => stanovi(i)
        WRITE (ID_File, 306) p_Vod%Get_Sifra(), p_Vod%Get_Naziv(), p_Stan%P_jednovremeno/1000, p_Vod%Ib, &
                             INT(p_Vod%presek), INT(p_Vod%In_odredjeno), p_Vod%pad_napona, p_Vod%struja_k_s
     ENDDO

     WRITE (ID_File, '(1x, 81("-") )' )
     WRITE (ID_File, '(1x, a, i2)') "#bakarni kabl PVC  #osigurac tipa gG  *na deonici TS-RT(n) zgrade", p_BrE%Get_Broj_Zgrada()
          
     
     305 FORMAT (1x, a, 3x, a, 4x, a, 3x, a, 4(2x, a))
     306 FORMAT (1x, i4, 2x, a8, 2x, f6.3, 2x, f6.2, 5x, i3, 10x, i3, 11x, f5.2, 6x, f7.2)


     
     !Ispis strujnih krugova
     
     WRITE (ID_File, '(//, 1x, 81("=") )' )
     WRITE (ID_File, '(27x, a)') "PRORACUN - STRUJNI KRUGOVI"
     WRITE (ID_File, '(1x, 81("=") )' ) 

     WRITE (ID_File, '(/, 1x, 81("-") )' )
     WRITE (ID_File, 307) "Sifra", "Br.Stana", "Naziv", "Ib[A]", "Presek[mm2]", "Osigurac[A]", "Uk.pad_U[%]", "Iks[A]*"
     WRITE (ID_File, '(1x, 81("-") )' )

     DO j = 1, p_BrE%Get_Broj_Stanova()        
        p_Stan => stanovi(j)
        DO i = 1, p_BrE%Get_Broj_Str_Krugova()        
            p_Str_krug => krugovi(i)
            
            IF( p_Str_krug%Get_Sifra_Stana() == p_Stan%Get_Sifra() ) THEN
                
                WRITE (ID_File, 308) p_Str_krug%Get_Sifra(), p_Stan%Get_Naziv(), p_Str_krug%Get_Naziv(), p_Str_krug%Ib, &
                                     p_Str_krug%presek, INT(p_Str_krug%In_odredjeno), p_Str_krug%pad_napona_TS_KRUG, &
                                     p_Str_krug%struja_k_s
            ENDIF

        ENDDO
        WRITE (ID_File, '(1x, 81("-"), / )' )
     ENDDO

     WRITE (ID_File, '(1x, a, i2)') "#bakarni kabl PVC  #osigurac tipa C  *na deonici TS-Potrosac(n) zgrade", p_BrE%Get_Broj_Zgrada()
     WRITE (ID_File, '(1x, 81("="), / )' )     
     
     307 FORMAT (1x, a, 1x, a, 4x, a, 5x, a, 4(2x, a))
     308 FORMAT (1x, i4, 2x, a7, 2x, a12, 1x, f5.2, 5x, f3.1, 11x, i2, 10x, f5.2, 4x, f7.2)


     !Provera uslova dozvoljenog pada napona

     DO i = 1, p_BrE%Get_Broj_Str_Krugova()        
        p_Str_krug => krugovi(i)
        IF ( p_Str_krug%pad_napona_TS_KRUG > 5. ) THEN
            WRITE(ID_File, '(a, i4, a, i2, a)') "[WARNING] Vrednost pada napona deonice TS-STRUJNI_KRUG ", p_Str_krug%Get_Sifra(), " zgrade", p_BrE%Get_Broj_Zgrada(), " nije u dozvoljenim granicama!"
            Pad_U_Error = .false.
        ENDIF        
     ENDDO

     IF(Pad_U_Error) WRITE(ID_File, *) "Vrednost pada napona svih deonica je u dozvoljenim granicama!"


    CLOSE(ID_File)
    
    
    !---------------------------------------------------------------------------------------------
    !Oslobadjanje dinamicke memorije
    
    IF( ALLOCATED(vodovi) ) DEALLOCATE( vodovi, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za vodove!")
    END IF

    IF( ALLOCATED(deonice_GRO_RT) ) DEALLOCATE( deonice_GRO_RT, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za deonice GRO-RT!")
    END IF

    IF( ALLOCATED(krugovi) ) DEALLOCATE( krugovi, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za strujne krugove!")
    END IF

    IF( ALLOCATED(stanovi) ) DEALLOCATE( stanovi, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za stanove!")
    END IF

    !---------------------------------------------------------------------------------------------


END SUBROUTINE Ispis_Dealokacija