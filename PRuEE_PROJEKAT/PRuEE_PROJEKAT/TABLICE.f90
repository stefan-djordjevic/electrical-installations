MODULE M_Tablice


IMPLICIT NONE

!NAZNACENE STRUJE OSIGURACA [A]
REAL :: In(14) = (/10., 16., 20., 25., 32., 40., 50., 63., 80., 100., 125., 160., 200., 250./) 


!provodnik od BAKRA (Cu), izolacije PVC *** elektricni razvod tipa C ***
!-------------------------------------------------------------------------------------------------------------------
REAL :: preseci_Cu(9) = (/1.5, 2.5, 4., 6., 10., 16., 25., 35., 50./) !PRESECI PROVODNIKA
REAL :: Itd_Cu(9) = (/17., 23., 31., 40., 54., 73., 95., 117., 141./) !TRAJNO DOZVOLJENE STRUJE (Itd)
REAL :: poduzne_rezistanse_Cu(9)=(/12.10, 7.41, 4.61, 3.08, 1.83, 1.15, 0.727, 0.524, 0.387/) !PODUZNE REZISTANSE  
REAL :: poduzne_reaktanse_Cu(9)=(/0.108, 0.104, 0.100, 0.094, 0.088, 0.083, 0.080, 0.077, 0.077/) !PODUZNE REAKTANSE, 3-zilni kabl


!provodnik od ALUMINIJUMA (Al), izolacije XPE *** elektricni razvod tipa D ***
!-------------------------------------------------------------------------------------------------------------------
REAL :: preseci_Al(9) = (/35., 50., 70., 95., 120., 150., 185., 240., 300./) !PRESECI PROVODNIKA
REAL :: Itd_Al(9) = (/94., 112., 138., 164., 186., 210., 236., 272., 308./)  !TRAJNO DOZVOLJENE STRUJE (Itd)
REAL :: poduzne_rezistanse_Al(9)=(/0.876, 0.604, 0.443, 0.320, 0.250, 0.198, 0.164, 0.128, 0.102/) !PODUZNE REZISTANSE
REAL :: poduzne_reaktanse_Al(9)=(/0.073, 0.072, 0.071, 0.069, 0.069, 0.069, 0.069, 0.069, 0.068/) !PODUZNE REAKTANSE, 3-zilni kabl


END MODULE M_Tablice