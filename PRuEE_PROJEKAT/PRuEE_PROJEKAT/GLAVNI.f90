PROGRAM Glavni_Program

USE M_Elementi_Ucitavanje
USE M_Globalni_Proracun
USE M_PRESEK_Proracun
USE M_Pad_Napona_i_Kratak_Spoj

IMPLICIT NONE

WRITE (*, *) "Program je pokrenut."

Global_Error = Ucitaj_Broj_Elemenata()
IF (Global_Error) STOP ("[ERROR] Problem prilikom ucitavanja podataka o broju elemenata!")

Global_Error = Ucitaj_Vodove()
IF (Global_Error) STOP ("[ERROR] Problem prilikom ucitavanja vodova!")

Global_Error = Ucitaj_deonice_GRO_RT()
IF (Global_Error) STOP ("[ERROR] Problem prilikom ucitavanja deonica GRO-RT!")

Global_Error = Ucitaj_strujne_krugove()
IF (Global_Error) STOP ("[ERROR] Problem prilikom ucitavanja strujnih krugova!")

Global_Error = Ucitaj_stanove()
IF (Global_Error) STOP ("[ERROR] Problem prilikom ucitavanja stanova!")


CALL Proracun_P_jednovremeno_STAN()

CALL Proracun_P_jednovremeno_ZGRADA()


CALL Proracun_Preseka_Svih_Deonica()


CALL Proracun_Pada_Napona_Vodova_i_KS_GRO()

CALL Proracun_Pada_Napona_Strujnih_Krugova()

CALL Proracun_Pada_Napona_GRO_RT_i_TS_Potr_i_KS_Potr()

WRITE (*, *)
WRITE (*, *) "Svi proracuni izvrseni."

CALL Ispis_Dealokacija()

WRITE (*, *)
WRITE (*, *) "Kraj."

END PROGRAM Glavni_Program