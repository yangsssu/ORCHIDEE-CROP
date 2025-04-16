
MODULE constantes_PaSim

  USE constantes

  LOGICAL, PARAMETER      :: blabla_pasim    = .FALSE. 
  LOGICAL, PARAMETER      :: DEBUG_anne      = .FALSE.
  LOGICAL, PARAMETER      :: BIG_DEBUG_anne  = .FALSE.
  INTEGER(i_std), PARAMETER      :: taille      = SELECTED_REAL_KIND(8)
  INTEGER(i_std), PARAMETER      :: taille_stdn = SELECTED_REAL_KIND(6,30)
  INTEGER(i_std), PARAMETER      :: nmaxprinc   = 8760
  INTEGER(i_std), PARAMETER      :: maxvar      = 100
  INTEGER(i_std), PARAMETER      :: NCUT        = 10
  INTEGER(i_std), PARAMETER      :: NSOILMAX    = 6
  INTEGER(i_std), PARAMETER      :: NFERT       = 10
  INTEGER(i_std), PARAMETER      :: NCANOPY     = 10
  INTEGER(i_std), PARAMETER      :: NGMEAN      = 10
  INTEGER(i_std), PARAMETER      :: NSTOCKING   = 10
  INTEGER(i_std), PARAMETER      :: NGAUSS      = 5
  INTEGER(i_std), PARAMETER      :: igrass      = 10
  LOGICAL, PARAMETER      :: Yearly_1snowzero          = .FALSE.
  REAL(r_std), PARAMETER :: minimum_vital             = 1e-30
  REAL(r_std), PARAMETER :: DEFAULTMaxt               = 0.02
  REAL(r_std), PARAMETER :: lambda                    = 2.49
  REAL(r_std), PARAMETER :: gamma                     = 0.065
  REAL(r_std), PARAMETER :: rho                       = 1.23
  REAL(R_std), PARAMETER :: alpha350                  = 0.145
  REAL(r_std), PARAMETER :: devsecond                 = 0.77
  REAL(R_std), PARAMETER :: sigma                     = 5.67e-8
  LOGICAL     , PARAMETER :: soilnh4pmobile            = .TRUE.
  REAL(R_std), PARAMETER :: nh3mol2nh3ug              = 17.0e6
  REAL(R_std), PARAMETER :: qm2mol                    = 44.6
  REAL(R_std), PARAMETER :: nn2omol2kg                = 0.028
  REAL(R_std), PARAMETER :: lf                        = 333750.0
  REAL(R_std), PARAMETER :: cp                        = 1.01e3
  REAL(R_std), PARAMETER :: rwt                       = 0
  REAL(R_std), PARAMETER :: dailysunshine             = 0
  REAL(R_std), PARAMETER :: pa2atm                    = 101325.0
  ! konvertierung von umol co2 nach kg c (kg c/umol co2)
  REAL(R_std), PARAMETER :: cumol2ckg                 = 12.0e-9 
  ! convertion de mol n en kg n 
  REAL(R_std), PARAMETER :: nmol2kg                   = 0.014 
  ! konvertierung von m nach mm (mm/m)  
  REAL(R_std), PARAMETER :: m2mm                      = 1000.0  
  REAL(R_std), PARAMETER :: highresoutput             = 0
  REAL(R_std), PARAMETER :: tfr                       = 273.15
  REAL(R_std), PARAMETER :: d0                        = 2.15e6
  REAL(R_std), PARAMETER :: nstructfix                = 0
  ! dichte von wasser @293.16k (kg h2o/m**3)
  REAL(R_std), PARAMETER :: rhow                      = 998.206  
  REAL(R_std), PARAMETER :: cw                        = 4.18e6
  REAL(R_std), PARAMETER :: kg2mg                     = 1.0e6
  ! anzahl sekunden pro tag (s/d)
  REAL(R_std), PARAMETER :: d2s                       = 86400   
  LOGICAL   , PARAMETER :: unanaerob                 = .TRUE.
  REAL(R_std), PARAMETER :: karman                    = 0.41
  REAL(R_std), PARAMETER :: dailyweatherdata          = 0
  REAL(R_std), PARAMETER :: rgas                      = 8.3143
  REAL(R_std), PARAMETER :: solar                     = 1370
  REAL(R_std), PARAMETER :: infinit                   = 1.0e20
  REAL(R_std), PARAMETER :: grav                      = 9.81
  ! ! konvertierung von l nach m**3 (m**3/l)
  REAL(R_std), PARAMETER :: l2m3                      = 1.0e-3 
  REAL(R_std), PARAMETER :: ug2kg                     = 1.0e-9
  REAL(R_std), PARAMETER :: cmol2kg                   = 0.012
  REAL(R_std), PARAMETER :: mw                        = 0.018
  REAL(R_std), PARAMETER :: mc                        = 28.5
  REAL(R_std), PARAMETER :: fcsh                      = 0.39
  REAL(R_std), PARAMETER :: ssta                      = 6.6
  REAL(R_std), PARAMETER :: fwnapo                    = 0.05
  REAL(R_std), PARAMETER :: mn                        = 62.0
  REAL(R_std), PARAMETER :: slam                      = 33.5   ! = 33.5
  REAL(R_std), PARAMETER :: fcr                       = 0.50
  REAL(R_std), PARAMETER :: tconstraintmax            = 10000.0
  REAL(R_std), PARAMETER :: nage                      = 4.0
  ! parameter for the calculation of enteric methane emission (kg CH4 (kg life weight)-1 d-1) 
  REAL(r_std), PARAMETER :: aCH4                      = 0.0002867   ! -0.0087  @equation constantes::aCH4
  ! parameter for the calculation of enteric methane emission (kg CH4 (kg life weight)-1 d-1)
  REAL(r_std), PARAMETER :: bCH4                      = 0.000045    ! 0.0541   @equation constantes::bCH4
  ! parameter for the calculation of enteric methane emission !!! 
  REAL(r_std), PARAMETER :: CH4toC                    = 0.75
  !        @equation constantes::CH4toC
  ! parameter for transform the biomass(gC/m2) to wsh (kgDM/m2) 
  ! for grass C ratio in Dry Matter usually 0.45 for trees usually 0.5
  REAL(r_std), PARAMETER :: CtoDM                    = 0.45  
  REAL(R_std), PARAMETER :: zeta                      = 10.0
  !facteur de conversion de MS a MO
  REAL(R_std), PARAMETER :: dm2om                     = 0.9    
  REAL(R_std), PARAMETER :: vcmaxadap                 = 1.0
  REAL(R_std), PARAMETER :: absorvl                   = 0.85
  ! fraction of net energy converted to metabolizable energy (-)
  REAL(Taille), PARAMETER :: k_CH4                     = 0.6   
  ! flag d'activation de l'effet des temperatures elevees sur l'ingestion    
  LOGICAL     , PARAMETER :: f_temperature_DMI         = .TRUE.
  ! TRUE : CH4 is calculated from N.Vuichard equation    
  LOGICAL     , PARAMETER :: f_CH4_methode             = .FALSE.   

  REAL(R_std), PARAMETER :: devear                    = 0.52
  REAL(r_std), PARAMETER :: nanimaltotmax             = 0.001
  REAL(r_std), PARAMETER :: tbase         = 278.15 !278.0
  REAL(r_std), PARAMETER :: trep          = 278.15 !278.0
  REAL(r_std), PARAMETER :: tasumrep      = 225.0

  ! variables for version 3.7 
  REAL(r_std), PARAMETER ::  l_min = 0.1
  REAL(r_std), PARAMETER ::  age_init_1 = 0
  REAL(r_std), PARAMETER ::  age_init_2 = 0
  REAL(r_std), PARAMETER ::  age_init_3 = 0
  REAL(r_std), PARAMETER ::  age_init_4 = 0

  REAL(R_std), PARAMETER :: fligninstructinit = 0.54
  REAL(r_std), PARAMETER :: fligninresidue    = 0.25

  REAL(R_std), PARAMETER :: devstagemin    = 0.6
  REAL(R_std), PARAMETER :: tgrowthmin     = 45 !30.0
  REAL(R_std), PARAMETER :: tgrowthmax     = 70.0
  REAL(R_std), PARAMETER :: misval         = -99.999
  REAL(R_std), PARAMETER :: gmeansloperel  = -0.002
  REAL(R_std), PARAMETER :: devstocking    = 0.001
  REAL(R_std), PARAMETER :: tseasonendmin  = 250.0

  ! constante for soil physics module
  REAL(r_std), PARAMETER    :: Temp_1csoil           = 2.4E6
  REAL(r_std), PARAMETER    :: Temp_1eta             = 0.6
  REAL(r_std), PARAMETER    :: sswmax                = 10000.0
  REAL(r_std), PARAMETER    :: tspring               = 100.0
  REAL(r_std), PARAMETER    :: tautomn               = 300.0

  ! constante for principal module
  REAL(R_std), PARAMETER :: psifc             = -612.
  REAL(R_std), PARAMETER :: psipwp            = -152905.

  ! constante for fertilisation module
  REAL(r_std), PARAMETER :: tapplmist        = 1.0
  REAL(r_std), PARAMETER :: fmistcmetabolic  = 0.7
  REAL(r_std), PARAMETER :: tapplvg          = 1.0
  REAL(r_std), PARAMETER :: asunshine        = 0.19
  REAL(r_std), PARAMETER :: fvgcmetabolic    = 0.8
  REAL(r_std), PARAMETER :: bsunshine        = 0.56
  REAL(r_std), PARAMETER :: tapplka          = 1.0
  REAL(r_std), PARAMETER :: c2nmist          = 15.0
  REAL(r_std), PARAMETER :: fvgnurine        = 0.61
  REAL(r_std), PARAMETER :: fkacmetabolic    = 0.9
  REAL(r_std), PARAMETER :: c2nkot           = 10.0
  REAL(r_std), PARAMETER :: fkanurine        = 0.9
  ! si DMI/DMIpot < intake_tolerance alors on sort les animaux ou on les complemente
  REAL(r_std), SAVE      :: intake_tolerance    = 0.80
  ! Maximum quantity of forage or concentrate to supplement animals when auto-supplementation (kg)
  REAL(r_std), SAVE      :: q_max_complement    = 6  

  INTEGER, SAVE :: f_saturant     = 0.
  INTEGER, SAVE :: f_nonlimitant  = 0.
  INTEGER, SAVE :: f_autogestion  = 0.     !080110 AIG fin
  INTEGER, SAVE :: f_complementation  = 0. !11 janvier 2009 AIG
  ! 0 : pas de fertilisation
  ! 1 : fertilisation geree par le modele pour un niveau de satisfaction des besoins en N
  INTEGER, SAVE :: f_fertilization = 0
  INTEGER, SAVE :: f_postauto = 0
  !071129 AIG
  ! constante for auto management modifications by nicolas vuichard
  !  INTEGER     , PARAMETER :: n_somitermin        = 2
  !  INTEGER     , PARAMETER :: n_somitermax        = 400
  !  REAL(Taille), PARAMETER :: nsatur_somerrormax  = 0.01
  ! Minimum threshold of Wshtot, below which animals are kept indoors (kg.m-2),
  ! either used when management is optimized by the model or for usual runs.
  REAL(r_std), PARAMETER :: min_grazing         = 0.11 
  !  INTEGER     , PARAMETER :: cte_nb_high_out     = 1
  !  INTEGER     , PARAMETER :: cte_nb_low_out      = 1
  !  INTEGER     , PARAMETER :: nb_last_year        = 1
  !20/03/2009 AIG & MG
  INTEGER, SAVE :: n_out          = 3 
  !Is the number of output for autogestion mangement run in import Yield file. 
  ! Dim1 : Fraction of grazed aera (F)   Dim2: Ratio F/(1-F)  Dim3: number of grazing days

END MODULE constantes_PaSim
